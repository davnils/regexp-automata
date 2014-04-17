module Main where

import           Automata.RegExp
import           Automata.DFA
import           Automata.NFA
import           Control.Monad (when)
import qualified Data.Set as S
import           Search.Lexer
import           Search.Parser

main :: IO ()
main = do
  (lang:regexp:input) <- fmap lines getContents
  let cp = Fix . parseRegExp . map fst $ alexScanTokens (".*" ++ regexp)
      nfa = exprToNFA (S.fromList lang) $ cp
      dfa = fromNFAToDFA nfa

  print cp
  print nfa
  print dfa

  let check str = do
        let res = simulate str dfa
        when res $ putStrLn str

  mapM_ check input
