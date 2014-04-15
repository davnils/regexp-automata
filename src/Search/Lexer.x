{
module Search.Lexer where

import Data.Char (isAlpha)

}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
  $white+                       ;

  "class"                       {tag' TClass                    }
  "public"                      {tag' TPublic                   }
  "static"                      {tag' TStatic                   }
  "main"                        {tag' TMain                     }
  "new"                         {tag' TNew                      }
  "return"                      {tag' TReturn                   }
  "if"                          {tag' TIf                       }
  "else"                        {tag' TElse                     }
  "while"                       {tag' TWhile                    }
  "System.out.println"          {tag' TPrint                    }
  "length"                      {tag' TLength                   }
  "this"                        {tag' TThis                     }

  "void"                        {tag' TVoid                     }
  "String"                      {tag' TString                   }
  "int"                         {tag' TInt                      }
  "boolean"                     {tag' TBoolean                  }
  "true"                        {tag' TTrue                     }
  "false"                       {tag' TFalse                    }

  "!"                           {tag' TNegation                 }
  "&&"                          {tag' TLogicAnd                 }
  "<"                           {tag' TCompareLess              }
  "+"                           {tag' TAdd                      }
  "-"                           {tag' TSub                      }
  "*"                           {tag' TMul                      }

  "="                           {tag' TAssignment               }
  ","                           {tag' TComma                    }
  "."                           {tag' TDot                      }
  ";"                           {tag' TSemiColon                }
  "("                           {tag' TLeftParen                }
  ")"                           {tag' TRightParen               }
  "["                           {tag' TLeftBracket              }
  "]"                           {tag' TRightBracket             }
  "{"                           {tag' TLeftBrace                }
  "}"                           {tag' TRightBrace               }
  "//".*                        {tag' TSingleLineComment        }
  "/*"[\x00-\x10ffff]*"*/"      {tag' TMultiLineComment         }

  [$alpha _][$alpha $digit _]*  {tag $ TIdLiteral               }
  0[lL]                         {tag $ TLongLiteral . readLong  }
  [1-9]$digit*[lL]              {tag $ TLongLiteral . readLong  }
  0                             {tag $ TIntLiteral  . read      }
  [1-9]$digit*                  {tag $ TIntLiteral  . read      }

{

data Token
 = TClass
 | TPublic
 | TStatic
 | TMain
 | TNew
 | TReturn
 | TIf
 | TElse
 | TWhile
 | TPrint
 | TLength
 | TThis

 | TIdLiteral String
 | TIntLiteral Int
 | TLongLiteral Int

 | TVoid
 | TString
 | TInt
 | TBoolean
 | TTrue
 | TFalse

 | TNegation
 | TLogicAnd
 | TCompareLess
 | TAdd
 | TSub
 | TMul

 | TAssignment
 | TComma
 | TDot
 | TSemiColon
 | TLeftParen
 | TRightParen
 | TLeftBracket
 | TRightBracket
 | TLeftBrace
 | TRightBrace
 | TSingleLineComment
 | TMultiLineComment
 deriving (Eq, Ord, Show)

type SourceInfo = (Int, Int)

readLong :: (Num a, Read a) => String -> a
readLong = read . takeWhile (not . isAlpha)

tag :: (String -> Token) -> AlexPosn -> String -> (Token, SourceInfo)
tag f (AlexPn _ row col) input =  (f input, (row, col))

tag' :: Token -> AlexPosn -> String -> (Token, SourceInfo)
tag' res pos =  tag (const res) pos

{-main = do
  s <- getContents
  print $ alexScanTokens s-}
}
