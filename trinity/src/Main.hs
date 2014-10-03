{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Main (main) where

import Control.Monad      ((=<<), (>>=))
import Data.Function      ((.))
import Data.Functor       ((<$>))
import System.Environment (getArgs)
import System.Exit        (exitFailure)
import System.IO          (hPutStrLn, print, readFile, stderr)

import Language.Trinity.Lexer  (scanTokens)
import Language.Trinity.Parser (parse)

main
  = getArgs
  >>= \ case
    [filename]
      → print
      . parse
      . scanTokens
      =<< readFile filename

    _
      → do
        hPutStrLn stderr "usage: $ ./trinity <file>.ty"
        exitFailure
