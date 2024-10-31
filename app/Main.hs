module Main (main) where

import           Control.Monad.IO.Class     (MonadIO (..))
import           Data.Bifunctor             (first)
import           Data.Proxy                 (Proxy (..))
import           Data.Text                  (Text)

import           Tiger.Amd64                (Gas (..))
import           Tiger.Codegen              (TempReg (..))
import           Tiger.EscapeAnalysis       (escapeAnalyze, getEscapeAnalysisResult)
import           Tiger.IR                   (IRData, canonicalize)
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
import qualified Tiger.Frame                as Frame
import qualified Tiger.RegMachine           as RegMachine

import           System.Exit                (exitFailure)

rightOrDie :: MonadIO m => (a -> m ()) -> Either a b -> m b
rightOrDie handler = \case
    Left err  -> handler err >> liftIO exitFailure
    Right res -> pure res

runInterpreter :: (FrameEmulator f e r s, Interpretable f e r s b)
               => ReturnRegister r
               -> FrameRegister r
               -> e
               -> IRData b f
               -> Text
               -> IO ()
runInterpreter rv fp emu ir input = do
    InterpreterResult{..} <- RegMachine.runInterpreter rv fp emu ir input
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
        irCfgInstr <- Frame.codegen irCfgStmt
        pure (irStmt, irCfgStmt, irCfgInstr)

    TIO.putStrLn "Type check was successful"
    LTIO.putStr $ Builder.toLazyText $ toTextBuilder irStmt
    runInterpreter (ReturnRegister RV) (FrameRegister FP) emu irStmt input
    LTIO.putStr $ Builder.toLazyText $ toTextBuilder irCfgStmt
    runInterpreter (ReturnRegister RV) (FrameRegister FP) emu irCfgStmt input
    LTIO.putStr $ Builder.toLazyText $ toTextBuilder $ first (fmap Gas) irCfgInstr

    tregEmu  <- newEmulator @Amd64.TempRegEmulator (Proxy @Amd64.LinuxFrame)
    runInterpreter (ReturnRegister $ Reg Amd64.Rax) (FrameRegister $ Reg Amd64.Rbp) tregEmu irCfgInstr input
  where input = "5\n10\n9\n8\n7\n6\n5"
