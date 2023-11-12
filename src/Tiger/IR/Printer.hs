module Tiger.IR.Printer
    ( irDataBuilder
    , irBuilder
    ) where

import           Data.Text.Lazy.Builder     (Builder)

import           Tiger.Frame
import           Tiger.IR.Types
import           Tiger.Temp                 (labelBuilder, tempBuilder)
import           Tiger.TextUtils

import qualified Data.Text.Lazy.Builder.Int as Builder

irDataBuilder :: Frame f => IRData f -> Builder
irDataBuilder IRData{..} = strings <> funcs
  where strings =
            let res = foldMap (\str -> labeledStringBuilder str <> "\n") resStrings
            in if null resStrings then "" else "(strings)\n" <> res <> "\n"

        funcs =  "(functions)\n"
              <> intercalate irFunctionBuilder "\n" resFunctions

        labeledStringBuilder :: LabeledString -> Builder
        labeledStringBuilder LabeledString{..} =  labelBuilder lStringLabel
                                               <> ": "
                                               <> stringBuilder lStringValue

        irFunctionBuilder :: Frame f => IRFunction f -> Builder
        irFunctionBuilder IRFunction{..} =
            let args = intercalate accessBuilder ", " (frameArgs irFuncFrame)
            in labelBuilder (frameName irFuncFrame)
            <> "(" <> args <> "):\n"
            <> foldMap (\i -> "    " <> irBuilder i <> "\n") irFuncBody

irBuilder :: IR a -> Builder
irBuilder = \case
    Const t -> Builder.decimal t
    Name l  -> labelBuilder l
    Temp t  -> tempBuilder t
    Assign dst val -> tempBuilder dst <> " ← " <> irBuilder val
    Binop dst op t1 t2 -> tempBuilder dst
                       <> " ← "
                       <> irBuilder t1 <> binopBuilder op <> irBuilder t2
    Load dst addr      -> tempBuilder dst <> " ← "<> deref addr
    Store addr src     -> deref addr <> " ← " <> tempBuilder src
    Label l            -> labelBuilder l <> ":"
    Jump l             -> "jmp " <> labelBuilder l
    CJump op t1 t2 t f -> "cjmp ("
                       <> irBuilder t1 <> relopBuilder op <> irBuilder t2
                       <> ") "
                       <> labelBuilder t <> " " <> labelBuilder f
    Call mdst f args   -> let call =  labelBuilder f
                                   <> "("
                                   <> intercalate irBuilder ", " args
                                   <> ")"
                          in maybe call (\dst -> tempBuilder dst <> " ← " <> call) mdst
    Ret mres           -> maybe "ret" (\res -> "ret " <> irBuilder res) mres
  where deref :: IR a -> Builder
        deref addr = "*" <> irBuilder addr

        binopBuilder :: Binop -> Builder
        binopBuilder = \case
            Add    -> " + "
            Sub    -> " - "
            Mul    -> " * "
            Div    -> " / "
            And    -> " & "
            Or     -> " | "
            Xor    -> " ^ "
            LShift -> " << "
            RShift -> " >> "

        relopBuilder :: Relop -> Builder
        relopBuilder = \case
            Eq  -> " == "
            Ne  -> " != "
            Lt  -> " < "
            Le  -> " <= "
            Gt  -> " > "
            Ge  -> " >= "
            ULt -> " u< "
            ULe -> " u<= "
            UGt -> " u> "
            UGe -> " u>= "




