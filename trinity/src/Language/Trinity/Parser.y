{
module Language.Trinity.Parser where

import Data.Sequence ((|>), Seq, singleton)
import Language.Trinity.Lexer
}

%name      parse
%tokentype { Token             }
%error     { parseError        }

%token
  "number" { TkNumber          }
  "row"    { TkRow             }
  "col"    { TkCol             }
  "matrix" { TkMatrix          }
  "print"  { TkPrint           }
  string   { TkString _        }
  "if"     { TkIf              }
  "then"   { TkThen            }
  "else"   { TkElse            }
  "end"    { TkEnd             }
  "while"  { TkWhile           }
  "do"     { TkDo              }
  "for"    { TkFor             }
  "in"     { TkIn              }
  "use"    { TkUse             }
  iden     { TkIden _          }
  number   { TkFloat _         }
  "="      { TkAssign          }
  ","      { TkComma           }
  ";"      { TkSemicolon       }
  ":"      { TkColon           }
  "("      { TkLParen          }
  ")"      { TkRParen          }
  "["      { TkLBrackets       }
  "]"      { TkRBrackets       }
  "{"      { TkLBraces         }
  "}"      { TkRBraces         }
  "not"    { TkNot             }
  "&"      { TkAnd             }
  "|"      { TkOr              }
  "=="     { TkEqual           }
  "/="     { TkUnequal         }
  "<"      { TkLess            }
  ">"      { TkGreat           }
  "<="     { TkLessEq          }
  ">="     { TkGreatEq         }
  "+"      { TkPlus            }
  "-"      { TkMinus           }
  "*"      { TkTimes           }
  "/"      { TkDivide          }
  "%"      { TkModulo          }
  "div"    { TkIntDivide       }
  "mod"    { TkIntModulo       }
  ".+."    { TkDottedPlus      }
  ".-."    { TkDottedMinus     }
  ".*."    { TkDottedTimes     }
  "./."    { TkDottedDivide    }
  ".%."    { TkDottedModulo    }
  ".div."  { TkDottedIntDivide }
  ".mod."  { TkDottedIntModulo }


%nonassoc "["
%left "*" "/" "%" "div" "mod"
%left "+" "-"
%left ".*." "./." ".%." ".div." ".mod."
%left ".+." ".-."
%nonassoc "==" "/=" "<" ">" "<=" ">="
%nonassoc "not"
%nonassoc "&"
%nonassoc "|"

%%

Program    : Instructions                                                        { $1                          }
Declaration: Type iden                                                           { DDefault           $1 (unTkIden $2) }
           | Type iden "=" Expression                                            { DInitialize        $1 (unTkIden $2) $4 }
Type       : "number"                                                            { TNumber                     }
           | "row"    "(" number ")"                                             { TRow               (unTkFloat $3) }
           | "col"    "(" number ")"                                             { TCol               (unTkFloat $3) }
           | "matrix" "(" number "," number ")"                                  { TMatrix            (unTkFloat $3) (unTkFloat $5) }
Instruction: Expression                                                          { IExpression        $1       }
           | "print" Prints                                                      { IPrint             $2       }
           | "if" Expression "then" Instructions "end"                           { IIf                $2 $4    }
           | "if" Expression "then" Instructions "else" Instructions "end"       { IIfElse            $2 $4 $6 }
           | "while" Expression "do" Instructions "end"                          { IWhile             $2 $4    }
           | "for" iden "in" Expression "do" Instructions "end"                  { IFor               (unTkIden $2) $4 $6 }
           | "use" Declarations "in" Instructions "end"                          { IBlock             $2 $4    }
Print      : string                                                              { PrintStringLiteral (unTkString $1) }
           | Expression                                                          { PrintExpression    $1       }
Expression : "(" Expression ")"                                                  { $2                          }
           | iden                                                                { EIdentifier        (unTkIden $1) }
           | iden "(" Arguments ")"                                              { ECall              (unTkIden $1) $3 }
           | number                                                              { ELitScalar            (unTkFloat $1) }
           | "{" Rows "}"                                                        { ELitMatrix         $2       }
           | Expression "[" Expression                "]"                        { EAccessVector      $1 $3    }
           | Expression "[" Expression "," Expression "]"                        { EAccessMatrix      $1 $3    }
           | Expression "not"   Expression                                       { ENot               $1       }
           | Expression "&"     Expression                                       { EAnd               $1 $3    }
           | Expression "|"     Expression                                       { EOr                $1 $3    }
           | Expression "=="    Expression                                       { EEQ                $1 $3    }
           | Expression "/="    Expression                                       { ENEQ               $1 $3    }
           | Expression "<"     Expression                                       { ELT                $1 $3    }
           | Expression ">"     Expression                                       { EGT                $1 $3    }
           | Expression "<="    Expression                                       { ELE                $1 $3    }
           | Expression ">="    Expression                                       { EGE                $1 $3    }
           | Expression "+"     Expression                                       { EAdd               $1 $3    }
           | Expression "-"     Expression                                       { ESubstract         $1 $3    }
           | Expression "*"     Expression                                       { EMultiply          $1 $3    }
           | Expression "/"     Expression                                       { EDivide            $1 $3    }
           | Expression "%"     Expression                                       { EModulo            $1 $3    }
           | Expression "div"   Expression                                       { EDivideInteger     $1 $3    }
           | Expression "mod"   Expression                                       { EModuloInteger     $1 $3    }
           | Expression ".+."   Expression                                       { EMapAdd            $1 $3    }
           | Expression ".-."   Expression                                       { EMapSubstract      $1 $3    }
           | Expression ".*."   Expression                                       { EMapMultiply       $1 $3    }
           | Expression "./."   Expression                                       { EMapDivide         $1 $3    }
           | Expression ".%."   Expression                                       { EMapModulo         $1 $3    }
           | Expression ".div." Expression                                       { EMapDivideInteger  $1 $3    }
           | Expression ".mod." Expression                                       { EMapModuloInteger  $1 $3    }

Declarations: Declarations     Declaration ";" { $1 |> $2 } | Declaration ";" { singleton $1 }
Instructions: Instructions     Instruction ";" { $1 |> $2 } | Instruction ";" { singleton $1 }
Prints      : Prints       "," Print           { $1 |> $3 } | Print           { singleton $1 }
Rows        : Rows         ":" Columns         { $1 |> $3 } | Columns         { singleton $1 }
Columns     : Columns      "," Expression      { $1 |> $3 } | Expression      { singleton $1 }
Arguments   : Arguments    "," Expression      { $1 |> $3 } | Expression      { singleton $1 }

{
parseError = error . ("welp: " ++) . show

type NumericLiteral = Float
type Identifier     = String

data Type
  = TNumber
  | TRow    NumericLiteral
  | TCol    NumericLiteral
  | TMatrix NumericLiteral NumericLiteral
  deriving (Eq, Read, Show)

type Declarations = Seq Declaration
data Declaration
  = DDefault    Type Identifier
  | DInitialize Type Identifier Expression
  deriving (Eq, Read, Show)

type Instructions = Seq Instruction
data Instruction
  = IExpression Expression
  | IPrint      (Seq Print)
  | IIf         Expression   Instructions
  | IIfElse     Expression   Instructions Instructions
  | IWhile      Expression   Instructions
  | IFor        String       Expression   Instructions
  | IBlock      Declarations Instructions
  deriving (Eq, Read, Show)

data Print
  = PrintStringLiteral String
  | PrintExpression    Expression
  deriving (Eq, Read, Show)

type Rows      = Seq Row
type Row       = Seq Expression
type Arguments = Seq Expression
data Expression
  = EIdentifier       Identifier
  | ECall             Identifier Arguments
  | ELitScalar        NumericLiteral
  | ELitMatrix        Rows
  | EAccessVector     Expression Expression
  | EAccessMatrix     Expression Expression
  | ENot              Expression
  | EAnd              Expression Expression
  | EOr               Expression Expression
  | EEQ               Expression Expression
  | ENEQ              Expression Expression
  | ELT               Expression Expression
  | EGT               Expression Expression
  | ELE               Expression Expression
  | EGE               Expression Expression
  | EAdd              Expression Expression
  | ESubstract        Expression Expression
  | EMultiply         Expression Expression
  | EDivide           Expression Expression
  | EModulo           Expression Expression
  | EDivideInteger    Expression Expression
  | EModuloInteger    Expression Expression
  | EMapAdd           Expression Expression
  | EMapSubstract     Expression Expression
  | EMapMultiply      Expression Expression
  | EMapDivide        Expression Expression
  | EMapModulo        Expression Expression
  | EMapDivideInteger Expression Expression
  | EMapModuloInteger Expression Expression
  | EUMinus           Expression
  deriving (Eq, Read, Show)
}
