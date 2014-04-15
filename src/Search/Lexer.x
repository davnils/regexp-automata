{
module Search.Lexer where

import Data.Char (isAlpha)

}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
  [\n\r]                        ;
  "("                           {tag' TLeftParen                }
  ")"                           {tag' TRightParen               }
  "."                           {tag' TAny                      }
  "|"                           {tag' TEither                   }
  "*"                           {tag' TKleeneStar               }
  "+"                           {tag' TPlus                     }
  "?"                           {tag' TOption                   }
  [\x00-\x10ffff]               {tag $ TChar . head             }

{

data Token
 = TLeftParen
 | TRightParen
 | TAny
 | TEither
 | TKleeneStar
 | TPlus
 | TOption
 | TChar Char
 deriving (Eq, Ord, Show)

type SourceInfo = (Int, Int)

tag :: (String -> Token) -> AlexPosn -> String -> (Token, SourceInfo)
tag f (AlexPn _ row col) input =  (f input, (row, col))

tag' :: Token -> AlexPosn -> String -> (Token, SourceInfo)
tag' res pos =  tag (const res) pos

}
