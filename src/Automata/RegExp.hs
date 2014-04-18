{-# LANGUAGE DeriveFunctor #-}

module Automata.RegExp
(RegAST, AST(..), Fix(..))
where

import Data.Functor.Foldable

type RegAST = Fix AST

data AST t
  = AAny
  | AEither t t
  | AConcat t t
  | AKleeneStar t
  | ALeastOne t
  | AOption t
  | AChar Char
  deriving (Eq, Functor, Show)
