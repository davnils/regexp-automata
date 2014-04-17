module Main where

import           Automata.RegExp
import           Automata.DFA
import           Automata.NFA
import qualified Data.Set as S
import           Search.Lexer
import           Search.Parser

main :: IO ()
main = do
  [lang, regexp, input] <- fmap lines getContents
  let compiled = parseRegExp . map fst $ alexScanTokens regexp
  print . simulate input . fromNFAToDFA . exprToNFA (S.fromList lang) $ Fix compiled
