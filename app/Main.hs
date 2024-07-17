module Main (main) where

import           Control.Monad.IO.Class     (MonadIO (..))
import           Data.Proxy                 (Proxy (..))
import           Data.Text                  (Text)
import           System.Exit                (exitFailure)

import           Tiger.EscapeAnalysis       (escapeAnalyze, getEscapeAnalysisResult)
import           Tiger.Expr                 (exprToText)
import           Tiger.IR                   (Emulator (..), FrameEmulator, IRData,
                                             Interpretable, InterpreterResult (..),
                                             canonicalize, interpreterErrorBuilder,
                                             irDataCFGText, irDataStmtText)
import           Tiger.Parser               (parse)
import           Tiger.Semant               (posedExceptionToText, semantAnalyze)
import           Tiger.Temp                 (InitLabel (..), InitTemp (..), runTempM)
import           Tiger.TextUtils            (lazyStringBuilder)

import qualified Data.Text.IO               as TIO
import qualified Data.Text.Lazy.Builder     as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder
import qualified Data.Text.Lazy.IO          as LTIO

import qualified Tiger.Amd64                as Amd64
import qualified Tiger.IR                   as IR

rightOrDie :: MonadIO m => (a -> m ()) -> Either a b -> m b
rightOrDie handler = \case
    Left err  -> handler err >> liftIO exitFailure
    Right res -> pure res

runInterpreter :: (FrameEmulator f e, Interpretable f e b)
               => e
               -> IRData b f
               -> Text
               -> IO ()
runInterpreter emu ir input = do
    InterpreterResult{..} <- IR.runInterpreter emu ir input
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

main :: IO ()
main = do
    src <- TIO.readFile "test.tig"
    expr <- rightOrDie TIO.putStr (parse "test.tig" src)
    escExpr <- escapeAnalyze expr
    LTIO.putStr $ exprToText (getEscapeAnalysisResult escExpr)
    emu <- liftIO $ newEmulator @Amd64.Emulator (Proxy @Amd64.Frame)
    (irStmt, irCfg) <- runTempM (InitTemp 0) (InitLabel 0) $ do
        irStmt <- semantAnalyze @Amd64.Frame "test.tig" escExpr >>=
            rightOrDie (liftIO . TIO.putStrLn . posedExceptionToText)
        (irStmt, ) <$> canonicalize irStmt
    TIO.putStrLn "Type check was successful"
    LTIO.putStr $ irDataStmtText irStmt
    runInterpreter emu irStmt "6"
    LTIO.putStr $ irDataCFGText irCfg
    runInterpreter emu irCfg "6"
