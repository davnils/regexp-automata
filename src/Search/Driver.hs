module Main where

import           Automata.RegExp
import           Automata.DFA
import           Automata.NFA
import           Control.Monad (when)
import qualified Data.Map as M
import qualified Data.Set as S
import           Search.Lexer
import           Search.Parser

import Search.Tests

-- | Read input on format language <newline> regexp <newline> str_1 <newline> ...
--   Will print str_i if regexp matches any substring.
--   Currently DFA minimization is disabled.
main :: IO ()
main = do
  (lang:regexp:input) <- fmap lines getContents

  let cp     = Fix . parseRegExp . map fst $ alexScanTokens (".*" ++ regexp)
      nfa    = exprToNFA (S.fromList lang) cp
      dfa    = fromNFAToDFA nfa

  -- _ <- equivalent dfa

  minDFA <- minimize dfa
  -- print minDFA
  -- print "END OF MIN DFA"

  mapM_ (check minDFA) input
  return ()
  where
  check dfa str = do
    let res = simulate str dfa
    when res $ putStrLn str
