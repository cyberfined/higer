{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import           Control.Monad              (when)
import           Data.Proxy                 (Proxy (..))
import           Tiger.EscapeAnalysis       (escapeAnalyze, getEscapeAnalysisResult)
import           Tiger.Expr                 (exprToText)
--import           Tiger.IR               (irDataBuilder)
import           Tiger.IR
import           Tiger.Parser               (parse)
import           Tiger.Semant               (posedExceptionToText, semantAnalyze)
import           Tiger.TextUtils            (lazyStringBuilder)

import qualified Data.Text.IO               as TIO
import qualified Data.Text.Lazy.Builder     as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder
import qualified Data.Text.Lazy.IO          as LTIO

import qualified Tiger.Amd64                as Amd64

main :: IO ()
main = do
    src <- TIO.readFile "test.tig"
    case parse "test.tig" src of
        Left err   -> TIO.putStr err
        Right expr -> do
            escExpr <- escapeAnalyze expr
            TIO.putStr $ exprToText (getEscapeAnalysisResult escExpr)
            semantAnalyze @Amd64.Frame "test.tig" escExpr >>= \case
                Left err     -> TIO.putStrLn (posedExceptionToText err)
                Right irCode -> do
                    TIO.putStrLn "Type check was successful"
                    LTIO.putStr $ Builder.toLazyText $ irDataBuilder irCode

                    let seqLabels = getSequentialLabels irCode
                    when (not (null seqLabels)) $
                        LTIO.putStrLn $ Builder.toLazyText $ "\nSequential labels:\n"
                                                           <> seqLabelsBuilder seqLabels

                    let seqJumps = getSequentialJumps irCode
                    when (not $ null seqJumps) $
                        LTIO.putStrLn $ Builder.toLazyText $ "\nSequential jumps:\n"
                                                           <> seqJumpsBuilder seqJumps

                    emu <- newEmulator @Amd64.Emulator (Proxy @Amd64.Frame)
                    InterpreterResult{..} <- runInterpreter emu irCode "one\none\none\none\none"
                    case resError of
                        Just err -> do
                            let resText =  Builder.toLazyText
                                        $ "\n"
                                        <> interpreterErrorBuilder err <> "\n"
                                        <> "output: " <> lazyStringBuilder resOutput
                                        <> "\ncode: " <> Builder.decimal resCode
                                        <> "\n"
                            LTIO.putStr resText
                        Nothing -> do
                            let resText = Builder.toLazyText
                                        $ "\noutput: " <> lazyStringBuilder resOutput
                                        <> "\ncode: " <> Builder.decimal resCode
                                        <> "\n"
                            LTIO.putStr resText
