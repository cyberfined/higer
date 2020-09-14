module Main where

import Tiger.Expr
import Tiger.Parser
import Tiger.TypeCheck
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    sourceCode <- TIO.readFile sourceFile
    case parseFromText sourceFile sourceCode of
        Left err -> print err
        Right untypedTree -> case typeCheck sourceCode untypedTree of
            Left err -> TIO.putStrLn err
            Right typedTree -> TIO.putStrLn $ showExpr typedTree
  where sourceFile = "test.tig"
