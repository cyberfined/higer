module Main (main) where

import           Tiger.EscapeAnalysis (escapeAnalyze, getEscapeAnalysisResult)
import           Tiger.Expr           (exprToText)
import           Tiger.IR             (irDataText)
import           Tiger.Parser         (parse)
import           Tiger.Semant         (posedExceptionToText, semantAnalyze)

import qualified Data.Text.IO         as TIO
import qualified Data.Text.Lazy.IO    as LTIO

import qualified Tiger.Amd64          as Amd64

main :: IO ()
main = do
    src <- TIO.readFile "test.tig"
    case parse "test.tig" src of
        Left err   -> TIO.putStr err
        Right expr -> do
            escExpr <- escapeAnalyze expr
            LTIO.putStr $ exprToText (getEscapeAnalysisResult escExpr)
            semantAnalyze @Amd64.Frame "test.tig" escExpr >>= \case
                Left err -> TIO.putStrLn (posedExceptionToText err)
                Right irCode -> do
                    TIO.putStrLn "Type check was successful"
                    LTIO.putStr $ irDataText irCode
