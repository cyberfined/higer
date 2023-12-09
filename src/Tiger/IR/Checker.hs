module Tiger.IR.Checker
    ( FuncSeqLabels(..)
    , getSequentialLabels
    , seqLabelsBuilder
    , seqLabelsText
    , FuncSeqJumps(..)
    , getSequentialJumps
    , seqJumpsBuilder
    , seqJumpsText
    ) where

import           Data.Text.Lazy         (Text)
import           Data.Text.Lazy.Builder (Builder)

import           Tiger.Frame            (Frame, frameName)
import           Tiger.IR.Printer       (irBuilder)
import           Tiger.IR.Types         (IR (..), IRData (..), IRFunction (..), Stmt)
import           Tiger.Temp             (Label, labelBuilder)
import           Tiger.TextUtils        (intercalate)

import qualified Data.Text.Lazy.Builder as Builder

data FuncSeqLabels = FuncSeqLabels
    { seqLabelsFunc :: !Label
    , seqLabelsList :: ![Label]
    }

getSequentialLabels :: Frame f => IRData f -> [FuncSeqLabels]
getSequentialLabels = foldr goFunc [] . resFunctions
  where goFunc IRFunction{..} xs = fst $ foldr (goStmt $ frameName irFuncFrame)
                                               (xs, False)
                                               irFuncBody

        goStmt :: Label
               -> IR Stmt
               -> ([FuncSeqLabels], Bool)
               -> ([FuncSeqLabels], Bool)
        goStmt fun (Label lbl) (xs, True) = case xs of
                  (FuncSeqLabels{..}:ys)
                      |  fun == seqLabelsFunc
                      -> (FuncSeqLabels fun (lbl : seqLabelsList) : ys, True)
                      |  otherwise
                      -> insNewFun
                  []  -> insNewFun
          where insNewFun = (FuncSeqLabels fun [lbl] : xs, True)
        goStmt _ (Label _) (xs, False) = (xs, True)
        goStmt _ _ (xs, _) = (xs, False)

seqLabelsBuilder :: [FuncSeqLabels] -> Builder
seqLabelsBuilder = intercalate builder "\n\n"
  where builder FuncSeqLabels{..} =  "func " <> labelBuilder seqLabelsFunc <> ":\n"
                                  <> intercalate labelBuilder "\n" seqLabelsList

seqLabelsText :: [FuncSeqLabels] -> Text
seqLabelsText = Builder.toLazyText . seqLabelsBuilder

data FuncSeqJumps = FuncSeqJumps
    { seqJumpsFunc :: !Label
    , seqJumpsList :: ![IR Stmt]
    }

getSequentialJumps :: Frame f => IRData f -> [FuncSeqJumps]
getSequentialJumps = foldr goFunc [] . resFunctions
  where goFunc IRFunction{..} xs = fst $ foldr (goStmt $ frameName irFuncFrame)
                                               (xs, False)
                                               irFuncBody
        goStmt :: Label
               -> IR Stmt
               -> ([FuncSeqJumps], Bool)
               -> ([FuncSeqJumps], Bool)
        goStmt fun jmp@(Jump{}) acc  = checkJmp fun jmp acc
        goStmt fun jmp@(CJump{}) acc = checkJmp fun jmp acc
        goStmt _ _ (xs, _)           = (xs, False)

        checkJmp fun jmp (xs, True) = case xs of
            (FuncSeqJumps{..}:ys)
                | fun == seqJumpsFunc
                -> (FuncSeqJumps fun (jmp : seqJumpsList) : ys, True)
                | otherwise
                -> insNewFun
            []  -> insNewFun
          where insNewFun = (FuncSeqJumps fun [jmp] : xs, True)
        checkJmp _ _ (xs, False) = (xs, True)

seqJumpsBuilder :: [FuncSeqJumps] -> Builder
seqJumpsBuilder = intercalate builder "\n\n"
  where builder FuncSeqJumps{..} = "func " <> labelBuilder seqJumpsFunc <> ":\n"
                                 <> intercalate irBuilder "\n" seqJumpsList

seqJumpsText :: [FuncSeqJumps] -> Text
seqJumpsText = Builder.toLazyText . seqJumpsBuilder
