{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, TupleSections #-}
module Automata.DFA
(DFA(..), equivalent, fromNFAToDFA, minimize, simulate)
where

import qualified Automata.NFA as N
import           Control.Monad (foldM,forM,when)
import           Control.Monad.Trans (lift)
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State as ST
import           Data.Foldable (forM_)
import           Data.Functor.Foldable (cata)
import           Data.List ((\\), foldl', intercalate, intersect, partition, sort, union, sortBy, groupBy, elem)
import Data.List (find)
import qualified Data.Map as M
import           Data.Ord
import           Data.Maybe (fromJust, fromMaybe)
import           Data.Semigroup ((<>), Semigroup)
import qualified Data.Set as S
import System.IO.Unsafe

type Symbol = Char

data DFA k
  = DFA
  {
    _alphabet     :: S.Set Symbol,
    _states       :: M.Map (k, Symbol) k,
    _startState   :: k,
    _endStates    :: S.Set k,
    _size         :: Int
  }
  deriving (Eq)

instance (Show a) => Show (DFA (S.Set a)) where
  show dfa = unlines $ [
    show (_startState dfa),
    "digraph finite_state_machine {",
    "rankdir=LR;",
    "size=\"10,10\"",
    "node [shape = doublecircle];" <> renderAccepted <> ";",
    "node [shape = circle];"] <>
    map renderEntry (M.toList $ _states dfa) <>
    ["}"]
    where
    renderAccepted                 = concatMap show' (S.toList $ _endStates dfa)
    renderEntry ((from, sym), end) = show' from <> " -> " <> show' end <>
                                     "[ label = \"" <> renderSymbol sym <> "\" ];"
    renderSymbol sym               = [sym]
    show' set                      = "\"q_{" <> intercalate "," (map (escape . show) $ S.toList set) <> "}\""
    escape ('\\':xs) = "\\\\" <> escape xs
    escape c  = c

-- add closure of starting state and call build, followed by post-processing
fromNFAToDFA :: N.EpsNFA -> DFA (S.Set Int)
fromNFAToDFA nfa = update . flip R.runReader nfa $ do
  fs <-  closure . S.singleton $ N._startState nfa
  ST.execStateT (build fs) (dfa fs)
  where
  dfa fs = DFA (N._alphabet nfa) M.empty fs S.empty 0

  update (DFA sig states start _ _) = DFA sig states start (S.fromList accept) (length allStates)
    where
    allStates = map fst $ M.keys states -- TODO: shouldn't all map values be added as well?
    accept = filter (\s -> S.intersection s (N._endStates nfa) /= S.empty) allStates 

type BuildM k = ST.StateT (DFA k) (R.Reader N.EpsNFA)

-- given closure, calculate all transitions, then recurse on all transitions (if any new)
build :: S.Set Int -> BuildM (S.Set Int) ()
build state = do
  nfa <- R.ask
  let sigma = S.toList $ N._alphabet nfa

  dfa <- ST.get
  case M.lookup (state, head sigma) (_states dfa) of
    Just _ -> return ()
    Nothing -> forM_ sigma $ \a -> do
      let target = S.unions $ map (\p -> M.findWithDefault S.empty (p, Right a) $ N._states nfa) (S.toList state)
      extended <- lift $ closure target
      ST.modify $ \(DFA s states start end sz) -> DFA s (M.insert (state, a) extended states) start end sz
      build extended

closure :: S.Set Int -> R.Reader N.EpsNFA (S.Set Int)
closure = ST.execStateT $ do
  nfa <- R.ask
  st' <- ST.get
  -- for each state, check if new w.r.t. eps-set, then add and call closure,..
  forM_ (M.filterWithKey (\(s, k) _ -> S.member s st' && k == Left N.Epsilon) $ N._states nfa) $ \layer -> do
    st <- ST.get
    when (S.difference layer st /= S.empty) $ do
      let new = S.union st layer
      lift (closure new) >>= ST.put

type Node = S.Set Int

-- find unique representation
-- the new states have names consisting of all states joined together
minimize :: DFA Node -> IO (DFA Node)
minimize dfa = do
  eq <- equivalent dfa
  return $ DFA (_alphabet dfa)
              (states eq)
              (convert $ startState eq)
              (S.fromList $ endStates eq)
              0
  where
    convert = toState . fromJust
    endStates = map toState . filter (or . map (\s -> S.member s (_endStates dfa)))
    startState = find (elem (_startState dfa))
    toState = S.unions

    -- for each block, consider all symbols, lookup the target block
    states eq = M.unions $ map (convertTrans) eq
      where
      convertTrans st = M.fromList $ zip (zip this alpha) (map (toState . findTrans) alpha)
        where
        some = head st
        state = toState st
        alpha = S.toList $ _alphabet dfa
        this = repeat state

        -- actually need to find the target block here
        findTrans sym = fromJust $ find (elem target) eq
          where target = fromJust $ M.lookup (some, sym) $ _states dfa

-- compute equivalence classes of states using the table fill method
-- need a general strategy for creating the list of equivalence classes
equivalent :: DFA Node -> IO [[Node]]
equivalent dfa = return $ go (allStates dfa)
  where
  go []     = []
  go (x:xs) = (x:p1) : go p2
    where
    (p1, p2) = partition (eqComp x) xs
    eqComp s1 s2 = S.notMember (sortPair (s1, s2)) (distinct dfa)

allStates dfa = sort . S.toList . S.fromList $ M.elems (_states dfa) <> (map fst $ M.keys (_states dfa))

allPairs dfa = [(p, q) | p <- allStates dfa, q <- allStates dfa, p < q]

-- construction outlined on pp. 160 "Introduction to automata theory[..]" (Hopcroft et al, 2014)
distinct :: DFA Node -> S.Set (Node, Node)
distinct dfa = distinguished
  where
  -- for each pair of states (p,q), symbols s:
  --   add (p,q) into the dep-list of {delta(p,a),delta(q,a)}
  deps              = foldl' add M.empty (allPairs dfa)
  add deps' (p, q)  = S.fold (\s d -> M.alter (insertion p q) (sortPair (get p s, get q s)) d) deps' (_alphabet dfa)
  insertion p q     = Just . S.insert (sortPair (p, q)) . fromMaybe S.empty
  get state sym     = fromJust $ M.lookup (state, sym) (_states dfa)

  -- create an initial queue of distinguished states, process until completion
  distinguished     = ST.execState (process initialQueue) (S.fromList initialQueue)

  -- create all ordered tuples of accept and non-accepting states as init queue
  initialQueue      = [sortPair (p, q) | p <- nonAccept, q <- S.toList (_endStates dfa)]
  nonAccept         = (allStates dfa) \\ S.toList (_endStates dfa)

  process []        = return ()
  process ((p,q):t) = do
    distinct <- ST.get

    let list = fromMaybe S.empty $ M.lookup (p,q) deps
        new = S.difference list distinct

    ST.put $ S.union distinct new
    process $ S.toList new <> t

-- utility
sortPair (p,q)
  | p < q     = (p, q)
  | otherwise = (q, p)

simulate :: (Eq k, Ord k) => String -> DFA k -> Bool
simulate str dfa = go (_startState dfa) str
  where
  go st []     = S.member st (_endStates dfa)
  go st (x:xs)
    | S.member st (_endStates dfa) = True
    | otherwise                    = go (fromJust $ M.lookup (st, x) (_states dfa)) xs
