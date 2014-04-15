{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable #-}

module Automata.RegExp where

import Data.Functor.Foldable hiding (Foldable)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

type RegAST = Fix AST

data AST t
  = AAny
  | AEither t t
  | AConcat t t
  | AKleeneStar t
  | AZeroMore t
  | AOption t
  | AChar Char
  deriving (Eq, Foldable, Functor, Show, Traversable)
