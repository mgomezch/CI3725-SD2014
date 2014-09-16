{
module Language.Trinity.Parser where

import Data.Sequence ((|>), Seq, singleton)
}

%name      parse
%tokentype { String     }
%error     { parseError }

%token
  "number" { "number" }
  "row"    { "row"    }
  "col"    { "col"    }
  "matrix" { "matrix" }
  "print"  { "print"  }
  "strlit" { "strlit" }
  "if"     { "if"     }
  "then"   { "then"   }
  "else"   { "else"   }
  "end"    { "end"    }
  "while"  { "while"  }
  "do"     { "do"     }
  "for"    { "for"    }
  "in"     { "in"     }
  "use"    { "use"    }
  "id"     { "id"     }
  "0"      { "0"      }
  "="      { "="      }
  ","      { ","      }
  ";"      { ";"      }
  ":"      { ":"      }
  "("      { "("      }
  ")"      { ")"      }
  "["      { "["      }
  "]"      { "]"      }
  "{"      { "{"      }
  "}"      { "}"      }
  "not"    { "not"    }
  "&"      { "&"      }
  "|"      { "|"      }
  "=="     { "=="     }
  "/="     { "/="     }
  "<"      { "<"      }
  ">"      { ">"      }
  "<="     { "<="     }
  ">="     { ">="     }
  "+"      { "+"      }
  "-"      { "-"      }
  "*"      { "*"      }
  "/"      { "/"      }
  "%"      { "%"      }
  "div"    { "div"    }
  "mod"    { "mod"    }
  ".+."    { ".+."    }
  ".-."    { ".-."    }
  ".*."    { ".*."    }
  "./."    { "./."    }
  ".%."    { ".%."    }
  ".div."  { ".div."  }
  ".mod."  { ".mod."  }

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

Program    : Instructions                                                  { $1                          }
Declaration: Type "id"                                                     { DDefault           $1 $2    }
           | Type "id" "=" Expression                                      { DInitialize        $1 $2 $4 }
Type       : "number"                                                      { TNumber                     }
           | "row"    "(" "0" ")"                                          { TRow               $3       }
           | "col"    "(" "0" ")"                                          { TCol               $3       }
           | "matrix" "(" "0" "," "0" ")"                                  { TMatrix            $3 $5    }
Instruction: Expression                                                    { IExpression        $1       }
           | "print" Prints                                                { IPrint             $2       }
           | "if" Expression "then" Instructions "end"                     { IIf                $2 $4    }
           | "if" Expression "then" Instructions "else" Instructions "end" { IIfElse            $2 $4 $6 }
           | "while" Expression "do" Instructions "end"                    { IWhile             $2 $4    }
           | "for" "id" "in" Expression "do" Instructions "end"            { IFor               $2 $4 $6 }
           | "use" Declarations "in" Instructions "end"                    { IBlock             $2 $4    }
Print      : "strlit"                                                      { PrintStringLiteral $1       }
           | Expression                                                    { PrintExpression    $1       }
Expression : "(" Expression ")"                                            { $2                          }
           | "id"                                                          { EIdentifier           $1    }
           | "id" "(" Arguments ")"                                        { ECall              $1 $3    }
           | "0"                                                           { ELitScalar            $1    }
           | "{" Rows "}"                                                  { ELitMatrix         $2       }
           | Expression "[" Expression                "]"                  { EAccessVector      $1 $3    }
           | Expression "[" Expression "," Expression "]"                  { EAccessMatrix      $1 $3    }
           | Expression "not"   Expression                                 { ENot               $1       }
           | Expression "&"     Expression                                 { EAnd               $1 $3    }
           | Expression "|"     Expression                                 { EOr                $1 $3    }
           | Expression "=="    Expression                                 { EEQ                $1 $3    }
           | Expression "/="    Expression                                 { ENEQ               $1 $3    }
           | Expression "<"     Expression                                 { ELT                $1 $3    }
           | Expression ">"     Expression                                 { EGT                $1 $3    }
           | Expression "<="    Expression                                 { ELE                $1 $3    }
           | Expression ">="    Expression                                 { EGE                $1 $3    }
           | Expression "+"     Expression                                 { EAdd               $1 $3    }
           | Expression "-"     Expression                                 { ESubstract         $1 $3    }
           | Expression "*"     Expression                                 { EMultiply          $1 $3    }
           | Expression "/"     Expression                                 { EDivide            $1 $3    }
           | Expression "%"     Expression                                 { EModulo            $1 $3    }
           | Expression "div"   Expression                                 { EDivideInteger     $1 $3    }
           | Expression "mod"   Expression                                 { EModuloInteger     $1 $3    }
           | Expression ".+."   Expression                                 { EMapAdd            $1 $3    }
           | Expression ".-."   Expression                                 { EMapSubstract      $1 $3    }
           | Expression ".*."   Expression                                 { EMapMultiply       $1 $3    }
           | Expression "./."   Expression                                 { EMapDivide         $1 $3    }
           | Expression ".%."   Expression                                 { EMapModulo         $1 $3    }
           | Expression ".div." Expression                                 { EMapDivideInteger  $1 $3    }
           | Expression ".mod." Expression                                 { EMapModuloInteger  $1 $3    }

Declarations: Declarations     Declaration ";" { $1 |> $2 } | Declaration ";" { singleton $1 }
Instructions: Instructions     Instruction ";" { $1 |> $2 } | Instruction ";" { singleton $1 }
Prints      : Prints       "," Print           { $1 |> $3 } | Print           { singleton $1 }
Rows        : Rows         ":" Columns         { $1 |> $3 } | Columns         { singleton $1 }
Columns     : Columns      "," Expression      { $1 |> $3 } | Expression      { singleton $1 }
Arguments   : Arguments    "," Expression      { $1 |> $3 } | Expression      { singleton $1 }

{
parseError = error . ("welp: " ++) . show

type NumericLiteral = String
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
  | IPrint      [Either String Expression]
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
