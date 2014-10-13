{
module Language.Trinity.Parser where

import Data.Sequence ((|>), empty, singleton)

import Language.Trinity.AST
import Language.Trinity.Lexer
import Language.Trinity.Lexer.Token
}

%name      parse
%tokentype { Token      }
%error     { parseError }

%token
  "program"  { TkProgram         }
  "function" { TkFunction        }
  "begin"    { TkBegin           }
  "return"   { TkReturn          }
  "number"   { TkNumber          }
  "boolean"  { TkBoolean         }
  "false"    { TkFalse           }
  "true"     { TkTrue            }
  "row"      { TkRow             }
  "col"      { TkCol             }
  "matrix"   { TkMatrix          }
  "print"    { TkPrint           }
  string     { TkString _        }
  "read"     { TkRead            }
  "if"       { TkIf              }
  "then"     { TkThen            }
  "else"     { TkElse            }
  "end"      { TkEnd             }
  "while"    { TkWhile           }
  "do"       { TkDo              }
  "for"      { TkFor             }
  "in"       { TkIn              }
  "use"      { TkUse             }
  identifier { TkIden _          }
  number     { TkLitNum _        }
  "set"      { TkSet             }
  "="        { TkAssign          }
  ","        { TkComma           }
  ";"        { TkSemicolon       }
  ":"        { TkColon           }
  "("        { TkLParen          }
  ")"        { TkRParen          }
  "["        { TkLBrackets       }
  "]"        { TkRBrackets       }
  "{"        { TkLBraces         }
  "}"        { TkRBraces         }
  "not"      { TkNot             }
  "&"        { TkAnd             }
  "|"        { TkOr              }
  "=="       { TkEqual           }
  "/="       { TkUnequal         }
  "<"        { TkLess            }
  ">"        { TkGreat           }
  "<="       { TkLessEq          }
  ">="       { TkGreatEq         }
  "+"        { TkPlus            }
  "-"        { TkMinus           }
  "*"        { TkTimes           }
  "/"        { TkDivide          }
  "%"        { TkModulo          }
  "div"      { TkIntDivide       }
  "mod"      { TkIntModulo       }
  ".+."      { TkDottedPlus      }
  ".-."      { TkDottedMinus     }
  ".*."      { TkDottedTimes     }
  "./."      { TkDottedDivide    }
  ".%."      { TkDottedModulo    }
  ".div."    { TkDottedIntDivide }
  ".mod."    { TkDottedIntModulo }
  "'"        { TkTranspose       }


%nonassoc "|"
%nonassoc "&"
%nonassoc "==" "/=" "<" ">" "<=" ">="
%right    "not"
%left     "+" "-" ".+." ".-."
%left     "*" "/" "%" "div" "mod" ".*." "./." ".%." ".div." ".mod."
%right    neg
%left     "[" "'"

%%

Program    : Functions "program" Instructions "end" ";"                                        { Program            $1 $3       }
Function   : "function" identifier "(" Parameters ")" "return" Type "begin" Instructions "end" { Function           $2 $4 $7 $9 }
Parameter  : Type identifier                                                                   { Parameter          $1 $2       }
Instruction: Expression                                                                        { IExpression        $1          }
           | "set" identifier "=" Expression                                                   { IAssignment        $2 $4       }
           | "set" identifier "[" Expression "]" "=" Expression                                { IVectorAssignment  $2 $4 $7    }
           | "set" identifier "[" Expression "," Expression "]" "=" Expression                 { IMatrixAssignment  $2 $4 $6 $9 }
           | "print" Prints                                                                    { IPrint             $2          }
           | "read" identifier                                                                 { IRead              $2          }
           | "if" Expression "then" Instructions "end"                                         { IIf                $2 $4       }
           | "if" Expression "then" Instructions "else" Instructions "end"                     { IIfElse            $2 $4 $6    }
           | "while" Expression "do" Instructions "end"                                        { IWhile             $2 $4       }
           | "for" identifier "in" Expression "do" Instructions "end"                          { IFor               $2 $4 $6    }
           | "use" Declarations "in" Instructions "end"                                        { IBlock             $2 $4       }
Declaration: Type identifier                                                                   { DDefault           $1 $2       }
           | Type identifier "=" Expression                                                    { DInitialize        $1 $2 $4    }
Type       : "number"                                                                          { TNumber                        }
           | "boolean"                                                                         { TBoolean                       }
           | "row"    "(" number ")"                                                           { TRow               $3          }
           | "col"    "(" number ")"                                                           { TCol               $3          }
           | "matrix" "(" number "," number ")"                                                { TMatrix            $3 $5       }
Print      : string                                                                            { PrintStringLiteral $1          }
           | Expression                                                                        { PrintExpression    $1          }
Expression : "(" Expression ")"                                                                { $2                             }
           | identifier                                                                        { EVariableUse       $1          }
           | identifier "(" Arguments ")"                                                      { ECall              $1 $3       }
           | "false"                                                                           { EFalse                         }
           | "true"                                                                            { ETrue                          }
           | number                                                                            { ELitScalar         $1          }
           | "{" Rows "}"                                                                      { ELitMatrix         $2          }
           | Expression "[" Expression                "]"                                      { EAccessVector      $1 $3       }
           | Expression "[" Expression "," Expression "]"                                      { EAccessMatrix      $1 $3 $5    }
           | "not" Expression                                                                  { ENot               $2          }
           | "-" Expression %prec neg                                                          { ENeg               $2          }
           | Expression "'"                                                                    { ETranspose         $1          }
           | Expression "&"     Expression                                                     { EAnd               $1 $3       }
           | Expression "|"     Expression                                                     { EOr                $1 $3       }
           | Expression "=="    Expression                                                     { EEQ                $1 $3       }
           | Expression "/="    Expression                                                     { ENEQ               $1 $3       }
           | Expression "<"     Expression                                                     { ELT                $1 $3       }
           | Expression ">"     Expression                                                     { EGT                $1 $3       }
           | Expression "<="    Expression                                                     { ELE                $1 $3       }
           | Expression ">="    Expression                                                     { EGE                $1 $3       }
           | Expression "+"     Expression                                                     { EAdd               $1 $3       }
           | Expression "-"     Expression                                                     { ESubstract         $1 $3       }
           | Expression "*"     Expression                                                     { EMultiply          $1 $3       }
           | Expression "/"     Expression                                                     { EDivide            $1 $3       }
           | Expression "%"     Expression                                                     { EModulo            $1 $3       }
           | Expression "div"   Expression                                                     { EDivideInteger     $1 $3       }
           | Expression "mod"   Expression                                                     { EModuloInteger     $1 $3       }
           | Expression ".+."   Expression                                                     { EMapAdd            $1 $3       }
           | Expression ".-."   Expression                                                     { EMapSubstract      $1 $3       }
           | Expression ".*."   Expression                                                     { EMapMultiply       $1 $3       }
           | Expression "./."   Expression                                                     { EMapDivide         $1 $3       }
           | Expression ".%."   Expression                                                     { EMapModulo         $1 $3       }
           | Expression ".div." Expression                                                     { EMapDivideInteger  $1 $3       }
           | Expression ".mod." Expression                                                     { EMapModuloInteger  $1 $3       }

Parameters: Parameters1 { $1 } | { empty }
Arguments : Arguments1  { $1 } | { empty }

Functions   : Functions        Function    ";" { $1 |> $2 } |            { empty        }
Instructions: Instructions     Instruction ";" { $1 |> $2 } |            { empty        }
Declarations: Declarations     Declaration ";" { $1 |> $2 } |            { empty        }
Parameters1 : Parameters   "," Parameter       { $1 |> $3 } | Parameter  { singleton $1 }
Arguments1  : Arguments    "," Expression      { $1 |> $3 } | Expression { singleton $1 }
Prints      : Prints       "," Print           { $1 |> $3 } | Print      { singleton $1 }
Rows        : Rows         ":" Columns         { $1 |> $3 } | Columns    { singleton $1 }
Columns     : Columns      "," Expression      { $1 |> $3 } | Expression { singleton $1 }

{
parseError = error . ("welp: " ++) . show
}
