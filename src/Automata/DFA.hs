{-# LANGUAGE FlexibleInstances, TupleSections #-}
module Automata.DFA
(DFA, fromNFAToDFA, minimize, simulate)
where

import qualified Automata.NFA as N
import           Control.Monad (forM,when)
import           Control.Monad.Trans (lift)
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State as ST
import           Data.Foldable (forM_)
import           Data.Functor.Foldable (cata)
import           Data.List ((\\), intercalate, intersect, partition, union)
import qualified Data.Map as M
import           Data.Maybe (fromJust)
import           Data.Semigroup ((<>), Semigroup)
import qualified Data.Set as S

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
-- minimize :: DFA Node -> DFA Node
minimize dfa = (merge eq dfa, equivalent dfa)
  where
  -- only keep distinct pairs, construct [[Node]] indicating equivalence classes
  eq = equivalent' dfa

  -- construct a single node for each equivalence class
  -- incoming edges to either node: add to replacement
  -- outgoing edges from either node: add to replacement
  -- self-references: add to replacement (i.e. looping on itself)
  merge [] dfa         = dfa
  merge (group:xs) dfa = merge xs (DFA (_alphabet dfa) (transferIn . transferOut $ _states dfa) newStart endStates 0)
    where
    newNode     = S.unions $ group <> [S.singleton $ -1]
    newStart    = if (elem (_startState dfa) group) then newNode else (_startState dfa)
    transferOut = M.map     $ \e -> if (elem e group) then newNode else e
    transferIn  = M.mapKeys $ \(src, sym) -> if (elem src group) then (newNode, sym) else (src, sym)

    -- delete all occurences of elements in group (set diff), and insert the new one (set union), if applicable
    -- assumes that either (i) all entries are non-accept (2) all entries are accept
    endStates   = if (removed == _endStates dfa) then (_endStates dfa) else (S.union (S.singleton newNode) removed)
    removed     = S.difference (_endStates dfa) (S.fromList $ map S.singleton $ S.toList newNode)

segment [] _         = []
segment (x:xs) pairs = [x : map snd emit] <> segment xs pairs'
  where
  (emit, pairs') = partition (\(a,b) -> a == x || b == x) pairs

equivalent' dfa = filter (\l -> length l > 1) . segment allStates . S.toList $ equivalent dfa
  where
  allStates = S.toList . S.fromList . map fst . M.keys $ _states dfa

sortTuple = map (\(x,y) -> if x <= y then (x,y) else (y, x))

-- locate inequivalent states (table fill method)
-- equivalent = all possible pairs - distinguished
equivalent :: DFA Node -> S.Set (Node, Node)
equivalent dfa = S.fromList $ [(p, q) | p <- allStates, q <- allStates, p < q] \\ go initial
  where
  initial   = sortTuple [(p, q) | p <- endStates, q <- allStates \\ endStates]
  endStates = S.toList $ _endStates dfa
  allStates = S.toList . S.fromList . map fst . M.keys $ _states dfa

  -- for every pair of disting.: for each a in alphabet: mark all source nodes as distinguished
  -- repeat until no more nodes have been added
  go :: [(Node, Node)] -> [(Node, Node)]
  go disting
    | length new == length disting = disting
    | otherwise                   = go new
    where
    new  = union disting $ concatMap consider disting

    -- for each alpha: check if there are incoming transitions, flag all source combinations
    consider (p, q) = concatMap (considerSymbol p q) (S.toList $ _alphabet dfa)
    considerSymbol p q s = removeDup [(a, b) | a <- concatMap (considerEntry p s) allStates, b <- concatMap (considerEntry q s) allStates, a /= b]
    removeDup = S.toList . S.fromList . sortTuple
    considerEntry dst sym src
      | M.lookup (src, sym) (_states dfa) == Just dst = [src]
      | otherwise = []

simulate :: (Eq k, Ord k) => String -> DFA k -> Bool
simulate str dfa = go (_startState dfa) str
  where
  go st []     = S.member st (_endStates dfa)
  go st (x:xs)
    | S.member st (_endStates dfa) = True
    | otherwise                    = go (fromJust $ M.lookup (st, x) (_states dfa)) xs
