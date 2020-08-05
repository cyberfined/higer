module Main where

import Parser
import TypeCheck
import EscapeAnalysis
import IR
import Optim
import Control.Monad(mapM_)
import Data.Graph

import Text.Parsec

data Amd64 = Amd64

instance Platform Amd64 where
    wordSize _ = 8

main :: IO ()
main = parseFromFile exprRoot "test.tig" >>= \res ->
    case res of
        Left err -> putStrLn err
        Right tree -> case runTypeCheck tree of
            Left err -> putStrLn err
            Right tree' -> let escapedTree = runEscapeAnalysis tree'
                               irst = runIRTranslation Amd64 escapedTree
                           in print escapedTree >>
                              mapM_ (\f -> putStr $ show f ++ "\n\n") (funDecs irst) >>
                              case runOptim irst of
                                  Left err -> putStrLn err
                                  Right optIrst -> mapM_ (\f -> putStr $ show f ++ "\n\n") (funDecs optIrst)
