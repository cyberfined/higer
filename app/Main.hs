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
import           Tiger.Semant               (semantAnalyze)
import           Tiger.Temp                 (InitLabel (..), InitTemp (..), Temp (..),
                                             runTempM)
import           Tiger.TextUtils            (TextBuildable (..), lazyStringBuilder)

import qualified Data.Text                  as Text
import qualified Data.Text.IO               as TIO
import qualified Data.Text.Lazy.Builder     as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder
import qualified Data.Text.Lazy.IO          as LTIO

import qualified Tiger.Amd64                as Amd64
import qualified Tiger.Frame                as Frame
import qualified Tiger.RegMachine           as RegMachine

import           System.Exit                (exitFailure)

import           Data.List                  (find)
import           Data.Maybe                 (fromJust)
import qualified Tiger.IR                   as IR
import           Tiger.RegAlloc             (liveRanges)
import qualified Tiger.Temp                 as Temp
import qualified Data.Graph.Inductive as Graph
import qualified Tiger.TextUtils as TextUtils

{-
import Tiger.RegAlloc (RegMap(..), Interval(..), RegState(..), getFreeReg, getTempInterval)
import qualified Tiger.EnumMap as EnumMap
import qualified Tiger.Temp as Temp
import qualified Tiger.Codegen as CG
import Data.Graph.Inductive (Gr)
import qualified Data.Graph.Inductive as Graph
import Data.List.NonEmpty (NonEmpty(..))
import Control.Monad (void)

testGr :: Gr Int ()
testGr = Graph.mkGraph
    [(1, 1), (2, 2), (3, 3), (4, 4), (5, 5), (6, 6), (7, 7)]
    [(1, 2, ()), (1, 3, ()), (2, 7, ()), (3, 4, ()), (4, 5, ()), (5, 4, ()), (5, 6, ()),
     (6, 3, ()), (6, 7, ())]

-- 6 -> 8
testRegMap :: RegMap (TempReg Amd64.Reg)
testRegMap = RegMap $ EnumMap.fromList
    [ ( Reg Amd64.Rax
      ,  RegState { stInterval = Interval 0 5, stFree = False } :|
          [ RegState { stInterval = Interval 6 10, stFree = True }
          , RegState { stInterval = Interval 11 20, stFree = False }
          ]
      )
    , ( Reg Amd64.R11
      , RegState { stInterval = Interval 5 8, stFree = True } :|
          [ RegState { stInterval = Interval 9 20, stFree = False }
          ]
      )
    , ( Reg Amd64.R12
      , RegState { stInterval = Interval 5 11, stFree = False } :|
          [ RegState { stInterval = Interval 12 20, stFree = True }
          ]
      )
    , ( CG.Temp (Temp.Temp 23)
      , RegState { stInterval = Interval 0 4, stFree = True } :|
          [ RegState { stInterval = Interval 10 20, stFree = True }
          ]
      )
    , ( CG.Temp (Temp.Temp 24)
      , RegState { stInterval = Interval 10 15, stFree = True } :| []
      )
    ]
-}

rightOrDie :: MonadIO m => (a -> m ()) -> Either a b -> m b
rightOrDie handler = \case
    Left err  -> handler err >> liftIO exitFailure
    Right res -> pure res

printTextBuildable :: (MonadIO m, TextBuildable a) => a -> m ()
printTextBuildable = liftIO . LTIO.putStrLn . Builder.toLazyText . toTextBuilder

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
        irStmt <- semantAnalyze @Amd64.LinuxFrame "test.tig" escExpr >>= rightOrDie printTextBuildable
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

    let unw = IR.irFuncBody $ fromJust $ find (\IR.IRFunction{..} -> Frame.frameName irFuncFrame == Temp.LabelText "addMat_0") $ IR.irFunctions irCfgInstr
    let regMap = liveRanges unw
    LTIO.putStrLn $ Builder.toLazyText $ toTextBuilder regMap

    let collectLabels (f, t, _) =
          let (_, _, b1, _) = fromJust $ fst $ Graph.match f (IR.cfgGraph unw)
              (_, _, b2, _) = fromJust $ fst $ Graph.match t (IR.cfgGraph unw)
          in "(" <> toTextBuilder (IR.blockLabel b1) <> ", " <> toTextBuilder (IR.blockLabel b2) <> ")"
    let edges = TextUtils.intercalate ", " $ map collectLabels $ Graph.labEdges $ IR.cfgGraph unw
    LTIO.putStrLn $ Builder.toLazyText edges
    
  where input = "6\n2 3\n4 5\n6 1\n11 45\n89 100\n345 787"
