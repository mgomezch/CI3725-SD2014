{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

module Language.Trinity.AST where

import Data.Aeson            (FromJSON(parseJSON), ToJSON(toJSON))
import Data.Aeson.TH         (SumEncoding(ObjectWithSingleField), defaultOptions, deriveJSON, sumEncoding)
import Data.Eq               (Eq)
import Data.Foldable         (toList)
import Data.Function.Unicode ((∘))
import Data.Functor          ((<$>), fmap)
import Data.List             (concat)
import Data.Sequence         (Seq, fromList)
import Data.Traversable      (traverse)
import Text.Read             (Read)
import Text.Show             (Show)

import Language.Trinity.Lexer (Token)



type NumericLiteral = Token
type Identifier     = Token

data Program
  = Program { functions :: Functions, main :: Instructions }
  deriving (Eq, Read, Show)

type Functions = Seq Function
data Function
  = Function { name :: Token, parameters :: Parameters, returnType :: Type, functionBody :: Instructions }
  deriving (Eq, Read, Show)

type Parameters = Seq Parameter
data Parameter
  = Parameter { parameterType :: Type, parameter :: Identifier }
  deriving (Eq, Read, Show)

data Type
  = TNumber
  | TBoolean
  | TRow     { rowCount :: NumericLiteral }
  | TCol     { columnCount :: NumericLiteral }
  | TMatrix  { rowCount, columnCount :: NumericLiteral }
  deriving (Eq, Read, Show)

type Declarations = Seq Declaration
data Declaration
  = DDefault    { type_ :: Type, identifier :: Identifier }
  | DInitialize { type_ :: Type, identifier :: Identifier, initializer :: Expression }
  deriving (Eq, Read, Show)

type Instructions = Seq Instruction
data Instruction
  = IExpression       { expression :: Expression }
  | IAssignment       { target :: Identifier, value :: Expression }
  | IVectorAssignment { target :: Identifier, targetRow :: Expression, value :: Expression }
  | IMatrixAssignment { target :: Identifier, targetRow, targetColumn :: Expression, value :: Expression }
  | IPrint            { prints :: Prints }
  | IRead             { target :: Identifier }
  | IIf               { condition :: Expression, onTrue :: Instructions }
  | IIfElse           { condition :: Expression, onTrue, onFalse :: Instructions }
  | IWhile            { condition :: Expression, body :: Instructions }
  | IFor              { counter :: Identifier, source :: Expression, body :: Instructions }
  | IBlock            { declarations :: Declarations, body :: Instructions }
  | IReturn           { expression :: Expression }
  deriving (Eq, Read, Show)

type Prints = Seq Print
data Print
  = PrintStringLiteral { printString :: Token }
  | PrintExpression    { printExpression :: Expression }
  deriving (Eq, Read, Show)

type Rows        = Seq Expressions
type Expressions = Seq Expression
data Expression
  = EVariableUse      { variable :: Identifier }
  | ECall             { function :: Identifier, arguments :: Expressions }
  | EFalse
  | ETrue
  | ELitScalar        { number :: NumericLiteral }
  | ELitMatrix        { rows :: Rows }
  | EAccessVector     { vector :: Expression, index :: Expression }
  | EAccessMatrix     { matrix :: Expression, row :: Expression, column :: Expression }
  | ENot              { operand :: Expression }
  | ENeg              { operand :: Expression }
  | EUMinus           { operand :: Expression }
  | ETranspose        { operand :: Expression }
  | EAnd              { leftOperand, rightOperand :: Expression }
  | EOr               { leftOperand, rightOperand :: Expression }
  | EEQ               { leftOperand, rightOperand :: Expression }
  | ENEQ              { leftOperand, rightOperand :: Expression }
  | ELT               { leftOperand, rightOperand :: Expression }
  | EGT               { leftOperand, rightOperand :: Expression }
  | ELE               { leftOperand, rightOperand :: Expression }
  | EGE               { leftOperand, rightOperand :: Expression }
  | EAdd              { leftOperand, rightOperand :: Expression }
  | ESubstract        { leftOperand, rightOperand :: Expression }
  | EMultiply         { leftOperand, rightOperand :: Expression }
  | EDivide           { leftOperand, rightOperand :: Expression }
  | EModulo           { leftOperand, rightOperand :: Expression }
  | EDivideInteger    { leftOperand, rightOperand :: Expression }
  | EModuloInteger    { leftOperand, rightOperand :: Expression }
  | EMapAdd           { leftOperand, rightOperand :: Expression }
  | EMapSubstract     { leftOperand, rightOperand :: Expression }
  | EMapMultiply      { leftOperand, rightOperand :: Expression }
  | EMapDivide        { leftOperand, rightOperand :: Expression }
  | EMapModulo        { leftOperand, rightOperand :: Expression }
  | EMapDivideInteger { leftOperand, rightOperand :: Expression }
  | EMapModuloInteger { leftOperand, rightOperand :: Expression }
  deriving (Eq, Read, Show)

instance ToJSON a ⇒ ToJSON (Seq a) where
  toJSON
    = toJSON
    ∘ toList

instance FromJSON a ⇒ FromJSON (Seq a) where
  parseJSON
    = fmap fromList
    ∘ parseJSON

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
      [ ''Program
      , ''Function
      , ''Parameter
      , ''Type
      , ''Declaration
      , ''Instruction
      , ''Print
      , ''Expression
      ]
