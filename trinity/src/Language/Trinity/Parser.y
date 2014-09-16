{
module Language.Trinity.Parser where

import Data.Sequence ((|>), Seq, singleton)
}

%name      parse
%tokentype { String     }
%error     { parseError }

%token
  "id"    { "id"    }
  "0"     { "0"     }
  ";"     { ";"     }
  "("     { "("     }
  ")"     { ")"     }
  "["     { "["     }
  "]"     { "]"     }
  "{"     { "{"     }
  "}"     { "}"     }
  ","     { ","     }
  "not"   { "not"   }
  "&"     { "&"     }
  "|"     { "|"     }
  "=="    { "=="    }
  "/="    { "/="    }
  "<"     { "<"     }
  ">"     { ">"     }
  "<="    { "<="    }
  ">="    { ">="    }
  "+"     { "+"     }
  "-"     { "-"     }
  "*"     { "*"     }
  "/"     { "/"     }
  "%"     { "%"     }
  "div"   { "div"   }
  "mod"   { "mod"   }
  ".+."   { ".+."   }
  ".-."   { ".-."   }
  ".*."   { ".*."   }
  "./."   { "./."   }
  ".%."   { ".%."   }
  ".div." { ".div." }
  ".mod." { ".mod." }

%left "["
%left "*" "/" "%" "div" "mod"
%left "+" "-"
%left ".*." "./." ".%." ".div." ".mod."
%left ".+." ".-."
%nonassoc "==" "/=" "<" ">" "<=" ">="
%nonassoc "not"
%nonassoc "&"
%nonassoc "|"

%%

Expr      : "id"                       { EIdentifier $1          }
          | "(" Expr ")"               { $2                      }
          | "0"                        { ELitScalar $1           }
          | "{" Rows "}"               { ELitMatrix $2           }
          | Expr "[" Expr          "]" { EAccessVector     $1 $3 }
          | Expr "[" Expr "," Expr "]" { EAccessMatrix     $1 $3 }
          | Expr "not"   Expr          { ENot              $1    }
          | Expr "&"     Expr          { EAnd              $1 $3 }
          | Expr "|"     Expr          { EOr               $1 $3 }
          | Expr "=="    Expr          { EEQ               $1 $3 }
          | Expr "/="    Expr          { ENEQ              $1 $3 }
          | Expr "<"     Expr          { ELT               $1 $3 }
          | Expr ">"     Expr          { EGT               $1 $3 }
          | Expr "<="    Expr          { ELE               $1 $3 }
          | Expr ">="    Expr          { EGE               $1 $3 }
          | Expr "+"     Expr          { EAdd              $1 $3 }
          | Expr "-"     Expr          { ESubstract        $1 $3 }
          | Expr "*"     Expr          { EMultiply         $1 $3 }
          | Expr "/"     Expr          { EDivide           $1 $3 }
          | Expr "%"     Expr          { EModulo           $1 $3 }
          | Expr "div"   Expr          { EDivideInteger    $1 $3 }
          | Expr "mod"   Expr          { EModuloInteger    $1 $3 }
          | Expr ".+."   Expr          { EMapAdd           $1 $3 }
          | Expr ".-."   Expr          { EMapSubstract     $1 $3 }
          | Expr ".*."   Expr          { EMapMultiply      $1 $3 }
          | Expr "./."   Expr          { EMapDivide        $1 $3 }
          | Expr ".%."   Expr          { EMapModulo        $1 $3 }
          | Expr ".div." Expr          { EMapDivideInteger $1 $3 }
          | Expr ".mod." Expr          { EMapModuloInteger $1 $3 }

Rows      : Rows ";" Exprs { $1 |> $3      }
          | Exprs          { singleton $1  }
Exprs     : Exprs Expr     { $1 |> $2      }
          | Expr           { singleton $1  }

{
parseError = error . ("welp: " ++) . show

type Row = Seq Expr

data Expr
  = EIdentifier String
  | ELitScalar String
  | ELitMatrix (Seq Row)
  | EAccessVector     Expr Expr
  | EAccessMatrix     Expr Expr
  | ENot              Expr
  | EAnd              Expr Expr
  | EOr               Expr Expr
  | EEQ               Expr Expr
  | ENEQ              Expr Expr
  | ELT               Expr Expr
  | EGT               Expr Expr
  | ELE               Expr Expr
  | EGE               Expr Expr
  | EAdd              Expr Expr
  | ESubstract        Expr Expr
  | EMultiply         Expr Expr
  | EDivide           Expr Expr
  | EModulo           Expr Expr
  | EDivideInteger    Expr Expr
  | EModuloInteger    Expr Expr
  | EMapPlus          Expr Expr
  | EMapMinus         Expr Expr
  | EMapMultiply      Expr Expr
  | EMapDivide        Expr Expr
  | EMapModulo        Expr Expr
  | EMapDivideInteger Expr Expr
  | EMapModuloInteger Expr Expr
  | EUMinus  Expr
  deriving (Eq, Read, Show)
}
