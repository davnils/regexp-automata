module Main where

import Automata.RegExp
import Search.Lexer
import Search.Parser

main :: IO ()
main = getContents >>= print . parseRegExp . map fst . alexScanTokens
