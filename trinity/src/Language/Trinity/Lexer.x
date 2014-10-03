{
module Language.Trinity.Lexer
    ( Token(..)
    , scanTokens
    , Position(..)
    ) where

import Prelude hiding (lex)

import Data.List       (intercalate, foldl')
import Data.List.Split (splitOn)


}

%wrapper "posn"

$digit = 0-9

$large = [A-Z]
$small = [a-z \_]
$alpha = [$small $large]

$idchar = [$alpha $digit]

-- Alex won't work if we write it directly in the @inside_string
$backslash = ["\\abfnrtv]

@inside_string  = ($printable # ["\\] | \\$backslash)

@iden  = $idchar+

@number         = $digit+(\.$digit+)?
@char           = \'$printable\'

@string         = \"@inside_string*\"
@string_error   = \"@inside_string*

--------------------------------------------------------------------------------

tokens :-

        -- Whitespace/Comments
        $white+         ;
        "#".*           ;

        -- Language
        "program"       { lex' TkProgram         }
        ";"             { lex' TkSemicolon       }
        ","             { lex' TkComma           }
        ":"             { lex' TkColon           }
        "return"        { lex' TkReturn          }
        "begin"         { lex' TkBegin           }
        "use"           { lex' TkUse             }
        "in"            { lex' TkIn              }
        "end"           { lex' TkEnd             }

        -- -- Brackets
        "("             { lex' TkLParen          }
        ")"             { lex' TkRParen          }
        "["             { lex' TkLBrackets       }
        "]"             { lex' TkRBrackets       }
        "{"             { lex' TkLBraces         }
        "}"             { lex' TkRBraces         }

        -- Types
        "number"        { lex' TkNumber          }
        "boolean"       { lex' TkBoolean         }
        "matrix"        { lex' TkMatrix          }
        "row"           { lex' TkRow             }
        "col"           { lex' TkCol             }

        -- Statements
        -- -- Declarations
        "set"           { lex' TkSet             }
        "="             { lex' TkAssign          }

        -- -- In/Out
        "read"          { lex' TkRead            }
        "print"         { lex' TkPrint           }

        -- -- Conditionals
        "if"            { lex' TkIf              }
        "then"          { lex' TkThen            }
        "else"          { lex' TkElse            }

        -- -- Loops
        "for"           { lex' TkFor             }
        "do"            { lex' TkDo              }

        "while"         { lex' TkWhile           }

        -- Expressions/Operators
        -- -- Literals
        @number         { lex  (TkFloat . read)  }
        "true"          { lex' (TkBool True)     }
        "false"         { lex' (TkBool False)    }
        -- -- -- Filtering newlines
        @string         { lex  (TkString . dropQuotationMarks 1 1 . filterBackSlash) }

        -- -- Arithmetic
        "+"             { lex' TkPlus            }
        "-"             { lex' TkMinus           }
        "*"             { lex' TkTimes           }
        "/"             { lex' TkDivide          }
        "%"             { lex' TkModulo          }
        "div"           { lex' TkIntDivide       }
        "mod"           { lex' TkIntModulo       }

        ".+."           { lex' TkDottedPlus      }
        ".-."           { lex' TkDottedMinus     }
        ".*."           { lex' TkDottedTimes     }
        "./."           { lex' TkDottedDivide    }
        ".%."           { lex' TkDottedModulo    }
        ".div."         { lex' TkDottedIntDivide }
        ".mod."         { lex' TkDottedIntModulo }

        -- -- Boolean
        "|"             { lex' TkOr              }
        "&"             { lex' TkAnd             }
        "not"           { lex' TkNot             }

        "=="            { lex' TkEqual           }
        "/="            { lex' TkUnequal         }

        "<"             { lex' TkLess            }
        ">"             { lex' TkGreat           }
        "<="            { lex' TkLessEq          }
        ">="            { lex' TkGreatEq         }

        -- -- Identifiers
        @iden           { lex TkIden             }

        -- Errors
        .               { lexPosn (\str -> TkError (head str))   }
        @string_error   { lexPosn (\str -> TkStringError (dropQuotationMarks 1 0 $ filterBackSlash str)) }

{

--------------------------------------------------------------------------------

data Token
    -- Language
    = TkSemicolon | TkComma | TkColon
    | TkReturn

    -- -- Blocks
    | TkBegin | TkUse | TkIn | TkEnd | TkProgram | TkFunction

    -- -- Brackets
    | TkLParen | TkRParen | TkLBrackets | TkRBrackets | TkLBraces | TkRBraces

    -- -- Types
    | TkNumber | TkBoolean | TkMatrix | TkRow | TkCol

    -- Statements
    | TkSet
    | TkAssign

    -- -- I/O
    | TkRead | TkPrint

    -- -- Conditional
    | TkIf | TkThen | TkElse

    -- -- Loops
    | TkWhile | TkFor | TkDo

    -- Expressions
    -- -- Literals
    | TkFloat  { unTkFloat  :: Float  }
    | TkBool   { unTkBool   :: Bool   }
    | TkString { unTkString :: String }

    -- Operators
    -- -- Arithmetic
    | TkPlus | TkMinus | TkTimes | TkDivide | TkModulo | TkIntDivide | TkIntModulo
    | TkDottedPlus | TkDottedMinus | TkDottedTimes | TkDottedDivide | TkDottedModulo | TkDottedIntDivide | TkDottedIntModulo

    -- -- Boolean
    | TkOr | TkAnd | TkNot
    | TkEqual | TkUnequal
    | TkLess | TkGreat | TkLessEq | TkGreatEq

    -- Identifiers
    | TkIden { unTkIden :: String }

    -- Interpreter
    | TkEOF
    | TkError       { unTkError :: Char,         tkPosn :: Position }
    | TkStringError { unTkStringError :: String, tkPosn :: Position }
    deriving (Eq, Read)

instance Show Token where
    show tk = case tk of
        TkSemicolon -> "Semicolon"
        TkComma -> "Comma"
        TkColon -> "Colon"
        TkReturn -> "Return"
        TkBegin -> "Begin"
        TkUse -> "Use"
        TkIn -> "In"
        TkEnd -> "End"
        TkProgram -> "Program"
        TkFunction -> "Function"
        TkLParen -> "LParen"
        TkRParen -> "RParen"
        TkLBrackets -> "LBrackets"
        TkRBrackets -> "RBrackets"
        TkLBraces -> "LBraces"
        TkRBraces -> "RBraces"
        TkNumber -> "Number"
        TkBoolean -> "Boolean"
        TkMatrix -> "Matrix"
        TkRow -> "Row"
        TkCol -> "Col"
        TkSet -> "Set"
        TkAssign -> "Assign"
        TkRead -> "Read"
        TkPrint -> "Print"
        TkIf -> "If"
        TkThen -> "Then"
        TkElse -> "Else"
        TkWhile -> "While"
        TkFor -> "For"
        TkDo -> "Do"
        TkFloat val -> "Float (" ++ show val ++ ")"
        TkBool val -> "Bool (" ++ show val ++ ")"
        TkString val -> "String (" ++ show val ++ ")"
        TkPlus -> "Plus"
        TkMinus -> "Minus"
        TkTimes -> "Times"
        TkDivide -> "Divide"
        TkModulo -> "Modulo"
        TkIntDivide -> "Int"
        TkIntModulo -> "Int"
        TkDottedPlus -> "Dotted plus"
        TkDottedMinus -> "Dotted minus"
        TkDottedTimes -> "Dotted times"
        TkDottedDivide -> "Dotted divide"
        TkDottedModulo -> "Dotted modulo"
        TkDottedIntDivide -> "Dotted int divide"
        TkDottedIntModulo -> "Dotted int modulo"
        TkOr -> "Or"
        TkAnd -> "And"
        TkNot -> "Not"
        TkEqual -> "Equal"
        TkUnequal -> "Unequal"
        TkLess -> "Less"
        TkGreat -> "Great"
        TkLessEq -> "Less"
        TkGreatEq -> "Great"
        TkIden idn -> "Iden (" ++ idn ++ ")"
        TkEOF -> "EOF"
        TkError val posn -> "en " ++ show posn ++ ": caracter inesperado " ++ show val
        TkStringError val posn -> "en " ++ show posn ++ ": string mal escrito " ++ show val


-- (Line, Column)
newtype Position
  = Posn (Int, Int)
  deriving (Eq, Read)

instance Show Position where
    show (Posn (r,c)) = "línea " ++ show r ++ ", columna " ++ show c
--------------------------------------------------------------------------------

filterBackSlash :: String -> String
filterBackSlash str = foldl' (flip replace) str chars
    where
        replace :: (Char, Char) -> String -> String
        replace (new, old) = intercalate [new] . splitOn ['\\', old]
        chars = [('\a', 'a'), ('\b', 'b'), ('\f', 'f'),
                 ('\n', 'n'), ('\r', 'r'), ('\t', 't'),
                 ('\v', 'v'), ('"', '"'), ('\\', '\\')]

dropQuotationMarks :: Int -> Int -> String -> String
dropQuotationMarks l r = reverse . drop r . reverse . drop l

toPosition :: AlexPosn -> Position
toPosition (AlexPn _ r c) = Posn (r,c)

lexPosn f p str = f str (toPosition p)

-- lex :: AlexPosn -> -> String -> Token
lex f p str = f str

lex' = lex . const

scanTokens = alexScanTokens

}
