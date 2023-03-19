module Main (main) where

import Tiger.Expr
import Tiger.Parser (parse)
import Tiger.Semant (semantAnalyze, posedExceptionToText)

import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    src <- TIO.readFile "test.tig"
    case parse "test.tig" src of
        Left err   -> TIO.putStr err
        Right expr -> do
            TIO.putStr $ exprToText expr
            semantAnalyze "test.tig" expr >>= \case
                Left err -> TIO.putStrLn (posedExceptionToText err)
                Right () -> TIO.putStrLn "Type check was successful"
