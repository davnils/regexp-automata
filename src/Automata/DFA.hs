{-# LANGUAGE FlexibleInstances, TupleSections #-}
module Automata.DFA
(DFA, fromNFAToDFA, simulate)
where

import qualified Automata.NFA as N
import           Control.Monad (forM,when)
import           Control.Monad.Trans (lift)
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State as ST
import           Data.Foldable (forM_)
import           Data.Functor.Foldable (cata)
import           Data.List (intercalate, intersect)
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
    "digraph finite_state_machine {",
    "rankdir=LR;",
    "size=\"10,10\"",
    "node [shape = doublecircle];" <> renderAccepted <> ";",
    "node [shape = circle];"] <>
    map renderEntry (M.toList $ _states dfa) <>
    ["}"]
    where
    renderAccepted                 = concatMap (show') (S.toList $ _endStates dfa)
    renderEntry ((from, sym), end) = show' from <> " -> " <> show' end <>
                                     "[ label = \"" <> renderSymbol sym <> "\" ];"
    renderSymbol sym               = [sym]
    show' set                      = "\"q_{" <> intercalate "," (map show $ S.toList set) <> "}\""

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

minimize :: DFA (S.Set Int) -> DFA (S.Set Int)
minimize = undefined

simulate :: (Eq k, Ord k) => String -> DFA k -> Bool
simulate str dfa = go (_startState dfa) str
  where
  go st []     = S.member st (_endStates dfa)
  go st (x:xs) = go (fromJust $ M.lookup (st, x) (_states dfa)) xs
