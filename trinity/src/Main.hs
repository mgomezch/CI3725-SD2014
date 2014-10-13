{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Main (main) where

import Control.Monad.Unicode ((=≪), (≫=))
import Data.Function         (($))
import Data.Function.Unicode ((∘))
import Data.Functor          ((<$>))
import Data.Monoid.Unicode   ((⊕))
import Data.Text.Encoding    (decodeUtf8)
import Data.Text.IO          (putStrLn)
import Data.Yaml             (encode)
import System.Environment    (getArgs, getProgName)
import System.Exit           (exitFailure)
import System.IO             (hPutStrLn, readFile, stderr)

import Language.Trinity.Lexer  (scanTokens)
import Language.Trinity.Parser (parse)

main
  = getArgs
  ≫= \ case
    [filename]
      → putStrLn
      ∘ decodeUtf8
      ∘ encode
      ∘ parse
      ∘ scanTokens
      =≪ readFile filename

    _
      → do
        programName ← getProgName
        hPutStrLn stderr $ "usage: " ⊕ programName ⊕ "<filename>"
        exitFailure
