{
module Language.Trinity.Lexer
    ( Token(..)
    , scanTokens
    ) where

import Prelude hiding (lex)

}

%wrapper "basic"

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
        .               { lex (TkError . head)   }
        @string_error   { lex (TkStringError . dropQuotationMarks 1 0 . filterBackSlash) }

{

--------------------------------------------------------------------------------

data Token
    -- Language
    = TkSemicolon | TkComma | TkColon
    | TkReturn

    -- -- Blocks
    | TkBegin | TkUse | TkIn | TkEnd

    -- -- Brackets
    | TkLParen | TkRParen | TkLBrackets | TkRBrackets | TkLBraces | TkRBraces

    -- -- Types
    | TkNumber | TkBoolean | TkMatrix | TkRow | TkCol

    -- Statements
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
    | TkError       { unTkError :: Char }
    | TkStringError { unTkStringError :: String }
    deriving (Eq, Show)

--------------------------------------------------------------------------------

filterBackSlash :: String -> String
filterBackSlash = foldr func []
    where
        func :: Char -> String -> String
        func c str
            | c == '\\' = case head str of
                'a' -> '\a' : tail str
                'b' -> '\b' : tail str
                'f' -> '\f' : tail str
                'n' -> '\n' : tail str
                'r' -> '\r' : tail str
                't' -> '\t' : tail str
                'v' -> '\v' : tail str
            | otherwise = c : str

dropQuotationMarks :: Int -> Int -> String -> String
dropQuotationMarks l r = reverse . drop r . reverse . drop l

lex f str = f str

lex' = lex . const

scanTokens = alexScanTokens

}
