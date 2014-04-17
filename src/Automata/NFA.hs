module Automata.NFA
-- (EpsNFA, exprToNFA)
where

import           Automata.RegExp
import           Data.Functor.Foldable (cata)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Semigroup ((<>), Semigroup)
import qualified Data.Set as S

data Epsilon
  = Epsilon
  deriving (Eq, Ord, Show)

type Symbol = Char
type TransitionEntry = (Int, Either Epsilon Symbol)

data EpsNFA
  = EpsNFA
  {
    _alphabet     :: S.Set Symbol,
    _states       :: M.Map TransitionEntry (S.Set Int),
    _startState   :: Int,
    _endStates    :: S.Set Int,
    _size         :: Int
  }
  deriving (Eq)

instance Semigroup EpsNFA where
  e1 <> e2 = EpsNFA (S.union (_alphabet e1) (_alphabet e2))
                    (trans1 <> trans2)
                    (_startState e1)
                    (S.map (+sz) $ _endStates e2)
                    (_size e1 + _size e2)
    where
    trans1  = M.alter (addSingleton sz) (sz - 1, Left Epsilon) $ _states e1
    trans2  = M.mapKeys (\(idx, sym) -> (idx + sz, sym)) $ M.map (S.map (+sz)) $ _states e2
    sz      = _size e1

instance Show EpsNFA where
  show nfa = unlines $ [
    "digraph finite_state_machine {",
    "rankdir=LR;",
    "size=\"10,10\"",
    "node [shape = doublecircle];" <> renderAccepted <> ";",
    "node [shape = circle];"] <>
    map renderEntry (M.toList $ _states nfa) <>
    ["}"]
    where
    renderAccepted                 = concatMap (("q_" <>) . show) (S.toList $ _endStates nfa)
    renderEntry ((from, sym), end) = unlines . map (output from sym) $ S.toList end
    output from sym target         = "q_" <> show from <> " -> " <> "q_" <> show target <>
                                     "[ label = \"" <> renderSymbol sym <> "\" ];"
    renderSymbol (Left Epsilon)    = "empty"
    renderSymbol (Right sym)       = [sym]

exprToNFA :: S.Set Symbol -> RegAST -> EpsNFA
exprToNFA st = cata (\e -> alg e st)

alg :: AST EpsNFA -> S.Set Symbol -> EpsNFA
alg AAny            s = EpsNFA s (connectAll 0 1 s) 0 (S.singleton 1) 2
alg (AEither e1 e2) _ = either' e1 e2
alg (AConcat e1 e2) _ = e1 <> e2
alg (AKleeneStar e) s = kleene e s
alg (ALeastOne e)   s = e <> (kleene e s)
alg (AOption e)     s = either' e (binary [Left Epsilon] s)
alg (AChar c)       s = binary [Right c] s

connectAll s1 s2      = S.fold (\a m -> M.insert (s1, Right a) (S.singleton s2) m) M.empty
binary list sym       = EpsNFA sym
                          (M.fromList $ map (\elem -> ((0, elem), S.singleton 1)) list)
                          0 (S.singleton 1) 2

either' e1 e2         = tie $ single <> e1 <> e2 <> single
  where
  tie (EpsNFA alpha states start accept sz) =
    (\m -> EpsNFA alpha m start accept sz) $
    M.alter (addSingleton $ sz - 1) (_size e1, Left Epsilon) $     -- add e1 -> single2
    M.alter (addSingleton $ firstLower) (0, Left Epsilon) $        -- add single1 -> e2
    M.adjust (S.delete firstLower) (_size e1, Left Epsilon) states -- remove e1 -> e2
  firstLower = _size e1 + 1

kleene e s            = tie $ single <> e <> single
  where
  tie (EpsNFA alpha states start accept sz) =
    (\m -> EpsNFA alpha m start accept sz) $
    M.alter (addSingleton $ 1) (sz - 2, Left Epsilon) $            -- add prev final -> single2
    M.alter (addSingleton $ sz - 1) (0, Left Epsilon) states       -- add single1 -> single2

single = EpsNFA S.empty M.empty 0 (S.singleton 0) 1
addSingleton target = Just . S.union (S.singleton target) . fromMaybe S.empty
