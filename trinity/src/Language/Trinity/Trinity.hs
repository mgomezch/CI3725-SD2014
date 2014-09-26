module Main where

import Language.Trinity.Lexer
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    input <- if null args
        then error "usage: $ ./trinity <file>.ty"
        else readFile $ head args
    let tks = scanTokens input
    let errors = filter (\tk -> case tk of
                            TkError _ _       -> True
                            TkStringError _ _ -> True
                            _                 -> False) tks
    mapM_ print (if null errors then tks else errors)
    putStrLn "done."
