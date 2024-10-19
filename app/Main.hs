module Main (main) where

import           Control.Monad.IO.Class     (MonadIO (..))
import           Data.Bifunctor             (first)
import           Data.Proxy                 (Proxy (..))
import           Data.Text                  (Text)
import           System.Exit                (exitFailure)

import           Tiger.Amd64                (Gas (..))
import           Tiger.Codegen              (codegen)
import           Tiger.EscapeAnalysis       (escapeAnalyze, getEscapeAnalysisResult)
import           Tiger.IR                   (IRData, Stmt, canonicalize)
import           Tiger.Parser               (parse)
import           Tiger.RegMachine           (Emulator (..), FrameEmulator,
                                             FrameRegister (..), Interpretable,
                                             InterpreterResult (..), ReturnRegister (..))
import           Tiger.Semant               (posedExceptionToText, semantAnalyze)
import           Tiger.Temp                 (InitLabel (..), InitTemp (..), Temp (..),
                                             runTempM)
import           Tiger.TextUtils            (TextBuildable (..), lazyStringBuilder)

import qualified Data.Text.IO               as TIO
import qualified Data.Text.Lazy.Builder     as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder
import qualified Data.Text.Lazy.IO          as LTIO

import qualified Tiger.Amd64                as Amd64
import qualified Tiger.RegMachine           as RegMachine

rightOrDie :: MonadIO m => (a -> m ()) -> Either a b -> m b
rightOrDie handler = \case
    Left err  -> handler err >> liftIO exitFailure
    Right res -> pure res

runInterpreter :: (FrameEmulator f e Temp Stmt, Interpretable f e Temp Stmt b)
               => e
               -> IRData b f
               -> Text
               -> IO ()
runInterpreter emu ir input = do
    InterpreterResult{..} <- RegMachine.runInterpreter (ReturnRegister RV) (FrameRegister FP) emu ir input
    case resError of
        Just err -> do
            let resText =  Builder.toLazyText
                        $ "\n"
                        <> toTextBuilder err <> "\n"
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
    LTIO.putStr $ Builder.toLazyText $ toTextBuilder (getEscapeAnalysisResult escExpr)
    emu <- newEmulator @Amd64.IREmulator (Proxy @Amd64.LinuxFrame)
    (irStmt, irCfgStmt, irCfgInstr) <- runTempM (InitTemp 0) (InitLabel 0) $ do
        irStmt <- semantAnalyze @Amd64.LinuxFrame "test.tig" escExpr >>=
            rightOrDie (liftIO . TIO.putStrLn . posedExceptionToText)
        irCfgStmt <- canonicalize irStmt
        irCfgInstr <- codegen irCfgStmt
        pure (irStmt, irCfgStmt, irCfgInstr)

    TIO.putStrLn "Type check was successful"
    LTIO.putStr $ Builder.toLazyText $ toTextBuilder irStmt
    runInterpreter emu irStmt "6"
    LTIO.putStr $ Builder.toLazyText $ toTextBuilder irCfgStmt
    runInterpreter emu irCfgStmt "6"
    LTIO.putStr $ Builder.toLazyText $ toTextBuilder $ first (fmap Gas) irCfgInstr

    {-
    tregEmu  <- newEmulator @Amd64.TempRegEmulator (Proxy @Amd64.LinuxFrame)
    amd64Res <- Amd64.runInterpreter tregEmu (ReturnRegister $ Reg Amd64.Rax) (FrameRegister $ Reg Amd64.Rbp) irCfgInstr "6"
    -}
