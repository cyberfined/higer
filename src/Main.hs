module Main where

import Parser
import TypeCheck
import EscapeAnalysis

import Text.Parsec

main :: IO ()
main = parseFromFile exprRoot "test.tig" >>= \res ->
    case res of
        Left err -> putStrLn err
        Right tree -> case runTypeCheck tree of
            Left err -> putStrLn err
            Right tree' -> print $ runEscapeAnalysis tree'
