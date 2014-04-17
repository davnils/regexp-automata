module Main where

import           Automata.RegExp
import           Automata.DFA
import           Automata.NFA
import           Control.Monad (when)
import qualified Data.Map as M
import qualified Data.Set as S
import           Search.Lexer
import           Search.Parser

test :: IO ()
test = do
  let binds  = [((0,1), 0), ((0,0), 1), ((1,1), 0), ((1,0), 3), ((3,0), 4), ((3,1), 5), ((4,0), 4), ((4,1), 5), ((5,0), 5), ((5,1), 5)]
      binds' = map (\((x, y), z) -> ((S.singleton x, head $ show y), S.singleton z)) binds
      dfa    = DFA (S.fromList "01") (M.fromList binds') (S.singleton 0) (S.singleton $ S.singleton 5) 0
      (minDFA, eq) = minimize dfa
      sep    = putStrLn "-----------------------------------------------"

  print dfa
  sep
  print minDFA
  sep
  print $ eq -- map (\(x, y) -> (head $ S.toList x, head $ S.toList y)) eq
  sep

test2 :: IO ()
test2 = do
  let binds  = [((0,0), 1), ((0,1), 5), ((1,0), 6), ((1,1), 2), ((2,0), 0), ((2,1), 2),
                ((3,0), 2), ((3,1), 6), ((4,0), 7), ((4,1), 5), ((5,0), 2), ((5,1), 6),
                ((6,0), 6), ((6,1), 4), ((7,0), 6), ((7,1), 2)]
      binds' = map (\((x, y), z) -> ((S.singleton x, head $ show y), S.singleton z)) binds
      dfa    = DFA (S.fromList "01") (M.fromList binds') (S.singleton 0) (S.singleton $ S.singleton 2) 0
      (minDFA, eq) = minimize dfa
      sep    = putStrLn "-----------------------------------------------"

  print dfa
  sep
  print minDFA
  sep
  print $ map (\(x,y) -> (head $ S.toList x, head $ S.toList y)) (S.toList eq) -- map (map $ head . S.toList) eq
  sep

main :: IO ()
main = do
  (lang:regexp:input) <- fmap lines getContents
  let cp     = Fix . parseRegExp . map fst $ alexScanTokens (".*" ++ regexp)
      nfa    = exprToNFA (S.fromList lang) $ cp
      dfa    = fromNFAToDFA nfa
      -- (minDFA, eq) = minimize dfa
      sep    = putStrLn "-----------------------------------------------"

  print cp
  sep
  print nfa
  sep
  print dfa
  sep
  -- print minDFA
  sep
  -- print eq
  sep

  let check str = do
        let res = simulate str dfa
        when res $ putStrLn str

  mapM_ check input
