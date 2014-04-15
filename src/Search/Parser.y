{
module Search.Parser where

import Automata.RegExp
import Data.Functor.Foldable (Fix(..))
import Search.Lexer

}

%name parseRegExp
%tokentype { Token }
%error { parserError }

%token 
  '('                         { TLeftParen         }
  ')'                         { TRightParen        }
  '.'                         { TAny               }
  '|'                         { TEither            }
  '*'                         { TKleeneStar        }
  '+'                         { TPlus              }
  '?'                         { TOption            }
  char                        { TChar $$           }

%%

Reg
  : '.'                       { AAny                          }
  | Reg '|' Reg               { AEither     (Fix $1) (Fix $3) }
  | Reg '*'                   { AKleeneStar (Fix $1)          }
  | Reg '+'                   { ALeastOne   (Fix $1)          }
  | Reg '?'                   { AOption     (Fix $1)          }
  | '(' Reg ')'               {             $2                }
  | Reg '?'                   { AOption     (Fix $1)          }
  | Reg Reg                   { AConcat     (Fix $1) (Fix $2) }
  | char                      { AChar       $1                }

{

parserError :: [Token] -> a
parserError tokens = error $ "Parse error, left over: " ++ concatMap show tokens

}
