module Automata.NFA
(EpsNFA, exprToNFA)
where

import           Automata.RegExp
import           Data.Functor.Foldable (cata)
import qualified Data.Map as M
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
    _states       :: M.Map TransitionEntry Int,
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
    trans1 = M.insert (sz, Left Epsilon) (sz + 1) $ _states e1
    trans2 = M.mapKeys (\(idx, sym) -> (idx + sz, sym)) $ M.map (+sz) $ _states e2
    sz     = _size e1

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

connectAll s1 s2      = S.fold (\a m -> M.insert (s1, Right a) s2 m) M.empty
binary list sym       = EpsNFA sym (M.fromList $ map (\elem -> ((0, elem), 1)) list) 0 (S.singleton 1) 2

-- TODO
either' e1 e2         = tie (single <> e1) (e2 <> single)
  where
  single = EpsNFA S.Set M.empty 0 (S.singleton 0) 1
  tie (EpsNFA sym trans start accept num) = undefined

-- TODO
kleene e s            = undefined
