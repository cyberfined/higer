module Main (main) where

import           Tiger.EscapeAnalysis (escapeAnalyze)
import           Tiger.Expr           (exprToText)
import           Tiger.Parser         (parse)
import           Tiger.Semant         (posedExceptionToText, semantAnalyze)

import qualified Data.Text.IO         as TIO

main :: IO ()
main = do
    src <- TIO.readFile "test.tig"
    case parse "test.tig" src of
        Left err   -> TIO.putStr err
        Right expr -> do
            escExpr <- escapeAnalyze expr
            TIO.putStr $ exprToText escExpr
            semantAnalyze "test.tig" escExpr >>= \case
                Left err -> TIO.putStrLn (posedExceptionToText err)
                Right () -> TIO.putStrLn "Type check was successful"
