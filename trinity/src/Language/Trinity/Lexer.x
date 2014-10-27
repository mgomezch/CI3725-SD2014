{
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UnicodeSyntax #-}

module Language.Trinity.Lexer
  ( Position(..)
  , Token(..)
  , scanTokens
  )
where

import Prelude hiding (lex)

import Data.Function         (const, flip)
import Data.Function.Unicode ((∘))
import Data.List             (head)
import Data.String           (String)

import Language.Trinity.Lexer.Token
}

%wrapper "posn"

-- Alex won't work if we write it directly in the @inside_string
$backslash = ["\\n]

@inside_string  = ($printable # ["\\] | \\$backslash)

@iden  = [a-zA-Z][a-zA-Z_0-9]*

@number         = [0-9]+(\.[0-9]+)?
@char           = \'$printable\'

@string         = \"@inside_string*\"
@string_error   = \"@inside_string*

tokens :-
  $white+;
  "#".*  ;
  "%"        { token TkModulo                     }
  "&"        { token TkAnd                        }
  "'"        { token TkTranspose                  }
  "("        { token TkLParen                     }
  ")"        { token TkRParen                     }
  "*"        { token TkTimes                      }
  "+"        { token TkPlus                       }
  ","        { token TkComma                      }
  "-"        { token TkMinus                      }
  ".%."      { token TkDottedModulo               }
  ".*."      { token TkDottedTimes                }
  ".+."      { token TkDottedPlus                 }
  ".-."      { token TkDottedMinus                }
  "./."      { token TkDottedDivide               }
  ".div."    { token TkDottedIntDivide            }
  ".mod."    { token TkDottedIntModulo            }
  "/"        { token TkDivide                     }
  "/="       { token TkUnequal                    }
  ":"        { token TkColon                      }
  ";"        { token TkSemicolon                  }
  "<"        { token TkLess                       }
  "<="       { token TkLessEq                     }
  "="        { token TkAssign                     }
  "=="       { token TkEqual                      }
  ">"        { token TkGreat                      }
  ">="       { token TkGreatEq                    }
  "["        { token TkLBrackets                  }
  "]"        { token TkRBrackets                  }
  "begin"    { token TkBegin                      }
  "boolean"  { token TkBoolean                    }
  "col"      { token TkCol                        }
  "div"      { token TkIntDivide                  }
  "do"       { token TkDo                         }
  "else"     { token TkElse                       }
  "end"      { token TkEnd                        }
  "false"    { token TkFalse                      }
  "for"      { token TkFor                        }
  "function" { token TkFunction                   }
  "if"       { token TkIf                         }
  "in"       { token TkIn                         }
  "matrix"   { token TkMatrix                     }
  "mod"      { token TkIntModulo                  }
  "not"      { token TkNot                        }
  "number"   { token TkNumber                     }
  "print"    { token TkPrint                      }
  "program"  { token TkProgram                    }
  "read"     { token TkRead                       }
  "return"   { token TkReturn                     }
  "row"      { token TkRow                        }
  "set"      { token TkSet                        }
  "then"     { token TkThen                       }
  "true"     { token TkTrue                       }
  "use"      { token TkUse                        }
  "while"    { token TkWhile                      }
  "{"        { token TkLBraces                    }
  "|"        { token TkOr                         }
  "}"        { token TkRBraces                    }
  @iden      { const TkIden                       }
  @number    { const TkLitNum                     }
  @string    { const TkString                     }
  .          { flip (TkError ∘ head) ∘ toPosition }

{
token ∷ Token → (AlexPosn → String → Token)
token
  = const
  ∘ const

scanTokens
  = alexScanTokens

toPosition ∷ AlexPosn → Position
toPosition
  (AlexPn _ line column)
  = Position line column
}
