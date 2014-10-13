{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module Language.Trinity.Lexer.Token where

import Data.Aeson.TH    (SumEncoding(ObjectWithSingleField), defaultOptions, deriveJSON, sumEncoding)
import Data.Char        (Char)
import Data.Eq          (Eq)
import Data.Functor     ((<$>))
import Data.Int         (Int)
import Data.List        (concat)
import Data.String      (String)
import Data.Traversable (traverse)
import Prelude          (Double)
import Text.Read        (Read)
import Text.Show        (Show)

type Line   = Int
type Column = Int
data Position
  = Position Line Column
  deriving (Eq, Read, Show)

data Token
  = TkAnd
  | TkAssign
  | TkBegin
  | TkBoolean
  | TkCol
  | TkColon
  | TkComma
  | TkDivide
  | TkDo
  | TkDottedDivide
  | TkDottedIntDivide
  | TkDottedIntModulo
  | TkDottedMinus
  | TkDottedModulo
  | TkDottedPlus
  | TkDottedTimes
  | TkEOF
  | TkElse
  | TkEnd
  | TkEqual
  | TkFalse
  | TkFor
  | TkFunction
  | TkGreat
  | TkGreatEq
  | TkIf
  | TkIn
  | TkIntDivide
  | TkIntModulo
  | TkLBraces
  | TkLBrackets
  | TkLParen
  | TkLess
  | TkLessEq
  | TkMatrix
  | TkMinus
  | TkModulo
  | TkNot
  | TkNumber
  | TkOr
  | TkPlus
  | TkPrint
  | TkProgram
  | TkRBraces
  | TkRBrackets
  | TkRParen
  | TkRead
  | TkReturn
  | TkRow
  | TkSemicolon
  | TkSet
  | TkThen
  | TkTimes
  | TkTranspose
  | TkTrue
  | TkUnequal
  | TkUse
  | TkWhile

  | TkLitNum { unTkLitNum ∷ String }
  | TkIden   { unTkIden   ∷ String }
  | TkString { unTkString ∷ String }

  | TkError { unTkError ∷ Char, tkPosn ∷ Position }
  deriving (Eq, Read, Show)

let
  options
    = defaultOptions
      { sumEncoding
        = ObjectWithSingleField
      }
  in
    concat
    <$> traverse
      (deriveJSON options)
      [ ''Position
      , ''Token
      ]
