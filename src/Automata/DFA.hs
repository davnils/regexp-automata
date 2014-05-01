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
import           Data.List ((\\), foldl', intercalate, intersect, partition, sort, union)
import qualified Data.Map as M
import           Data.Maybe (fromJust)
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
    allStates = map fst $ M.keys states
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

-- find the unique (up to isomorphism) minimal representation
minimize :: DFA Node -> IO (DFA Node)
minimize dfa = do
  eq <- equivalent dfa
  return $ merge eq dfa
  where
  -- construct a single node for each equivalence class
  -- incoming edges to either node: add to replacement
  -- outgoing edges from either node: add to replacement
  -- self-references: add to replacement (i.e. looping on itself)
  merge [] dfa         = dfa
  merge (group:xs) dfa = merge xs (DFA (_alphabet dfa) (transferIn . transferOut $ _states dfa) newStart endStates 0)
    where
    newNode     = S.unions $ group <> [S.singleton $ -1]
    newStart    = if elem (_startState dfa) group then newNode else (_startState dfa)
    transferOut = M.map     $ \e -> if (elem e group) then newNode else e
    transferIn  = M.mapKeys $ \(src, sym) -> if (elem src group) then (newNode, sym) else (src, sym)

    -- assumes only one end state
    endStates   = if S.null $ S.intersection (_endStates dfa) $ S.fromList group then _endStates dfa else S.singleton newNode

-- compute equivalence classes of states using the table fill method
-- need a general strategy for creating the list of equivalence classes
equivalent :: DFA Node -> IO [[Node]]
equivalent dfa = do
  -- print dfa
  -- print "END OF DFA"
  -- print . S.toList $ distinct dfa
  -- print "END OF DISTINCT PAIRS"
  -- print eqPairs
  -- print "END OF EQUIVALENT PAIRS"
  -- print unassigned
  -- print "END OF UNASSIGNED"
  let res = ST.evalState buildClass unassigned
  -- print res
  -- print "END OF RESULT"
  return res
  where
  -- take all state pairs (with p < q) and remove all distinguishable
  eqPairs    = allPairs dfa \\ (S.toList $ distinct dfa)
  unassigned = allStates dfa
  buildClass = do
    (left :: [Node]) <- ST.get

    case left of
      [] -> return [[]]
      [x] -> return [[x]]
      (h:t) -> do
        -- build closure
        new <- closure' h
        rest <- buildClass
        return (new:rest)

  -- build a list of all reachable elements from 'elem'
  -- closure' :: Node -> [Node]
  closure' elem = do
    let pairs = filter (\(x,y) -> x == elem || y == elem) eqPairs
        new   = map (\(x,y) -> if x == elem then y else x) pairs
        new'  = elem:new

    prev <- ST.get
    let newState = prev \\ new'
    if newState == prev then
      return []
      else do
        ST.put newState
        rest <- fmap concat $ mapM closure' new
        return $ new' <> rest

allStates dfa = sort . S.toList . S.fromList $ M.elems (_states dfa) <> (map fst $ M.keys (_states dfa))
allPairs dfa = [(p, q) | p <- allStates dfa, q <- allStates dfa, p < q]

-- construction outlined on pp. 160 "Introduction to automata theory[..]" (Hopcroft et al, 2014)
distinct :: DFA Node -> S.Set (Node, Node)
distinct dfa = {-unsafePerformIO $ printDebug >> return -}distinguished
  where
  -- for each pair of states, intialize a list of dependencies, as follows:
  --
  -- for each pair of states (p,q):
  --   for all symbols s:
  --     add (p,q) into the dep-list of {delta(p,a),delta(q,a)}
  initDeps          = M.fromList . zip (allPairs dfa) $ repeat []
  deps              = foldl' add initDeps (allPairs dfa)
  add deps' (p, q)  = S.fold (\s d -> M.adjust ((sortPair (p, q)):) (sortPair (get p s, get q s)) d) deps' (_alphabet dfa)
  get state sym     = (\(Just val) -> val) $ M.lookup (state, sym) (_states dfa)

  -- create an initial queue of distinguished states, process until completion
  distinguished     = ST.execState (process initialQueue) (S.fromList initialQueue)

  -- create all ordered tuples of accept and non-accepting states as init queue
  initialQueue      = [sortPair (p, q) | p <- nonAccept, q <- S.toList (_endStates dfa)]
  nonAccept = (allStates dfa) \\ S.toList (_endStates dfa)

  {-printDebug = do
    putStrLn ""
    putStrLn $ "this is the deplist: " <> show deps
    putStrLn ""-}

  process []        = return ()
  process ((p,q):t) = do
    distinct <- ST.get
    case M.lookup (p,q) deps of
      Just [] -> process t
      Just list -> do
        let new = filter (not . flip S.member distinct) list
        ST.put $ S.union distinct (S.fromList list)
        process (new <> t)

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
