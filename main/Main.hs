module Main where

import Tiger.Expr
import Tiger.Parser
import Tiger.TypeCheck
import Tiger.EscapeAnalysis
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    sourceCode <- TIO.readFile sourceFile
    case parseFromText sourceFile sourceCode of
        Left err -> putStrLn err
        Right untypedTree -> case typeCheck sourceCode untypedTree of
            Left err -> TIO.putStrLn err
            Right typedTree -> do
                let escapedTree = escapeAnalysis typedTree
                TIO.putStrLn $ showExpr escapedTree
  where sourceFile = "test.tig"
