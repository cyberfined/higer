module Tiger.IR.Printer
    ( irDataText
    , irDataBuilder
    , stmtText
    , stmtBuilder
    ) where

import           Data.Text.Lazy.Builder     (Builder)

import           Tiger.Frame
import           Tiger.IR.Types
import           Tiger.Temp                 (labelBuilder, tempBuilder)
import           Tiger.TextUtils

import qualified Data.List.NonEmpty         as NonEmpty
import qualified Data.Text.Lazy             as LazyText
import qualified Data.Text.Lazy.Builder     as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder

import qualified Tiger.Temp                 as Temp

irDataText :: Frame f => IRData f -> LazyText.Text
irDataText = Builder.toLazyText . irDataBuilder

irDataBuilder :: Frame f => IRData f -> Builder
irDataBuilder IRData{..} = strings <> funcs
  where strings =
            let res = foldMap (\str -> labeledStringBuilder str <> "\n") irStrings
            in if null irStrings then "" else "(strings)\n" <> res <> "\n"
        funcs =  "(functions)\n"
              <> intercalate "\n" (map irFunctionBuilder irFunctions)

        labeledStringBuilder :: LabeledString -> Builder
        labeledStringBuilder LabeledString{..} =  labelBuilder lStringLabel
                                               <> ": "
                                               <> stringBuilder lStringValue

        irFunctionBuilder :: Frame f => IRFunction f -> Builder
        irFunctionBuilder IRFunction{..} =
            let args = intercalate ", " (map accessBuilder $ frameArgs irFuncFrame)
            in labelBuilder (frameName irFuncFrame)
            <> "(" <> args <> "):\n"
            <> stmtBuilder irFuncBody

stmtText :: Stmt -> LazyText.Text
stmtText = Builder.toLazyText . stmtBuilder

stmtBuilder :: Stmt -> Builder
stmtBuilder s = stmtBuilder' s "" ""
  where stmtBuilder' :: Stmt -> Builder -> Builder -> Builder
        stmtBuilder' = \case
            Move dst src -> showNode "move" [exprBuilder' dst, exprBuilder' src]
            Expr e -> exprBuilder' e
            Jump l -> showLeaf $ "jump " <> labelBuilder l
            CJump op e1 e2 tLab fLab ->
                showNodeNames ("cjump " <> relopBuilder op) $
                    [ (Nothing, exprBuilder' e1)
                    , (Nothing, exprBuilder' e2)
                    , (Just "true", labelBuilder' tLab)
                    , (Just "false", labelBuilder' fLab)
                    ]
            Seq es -> showNode ";" $ map stmtBuilder' $ NonEmpty.toList es
            Label l -> showLeaf $ labelBuilder l <> ":"

        exprBuilder' :: Expr -> Builder -> Builder -> Builder
        exprBuilder' = \case
            Const i -> showLeaf $ Builder.decimal i
            Name l  -> showLeaf $ labelBuilder l
            Temp t  -> showLeaf $ tempBuilder t
            Binop op e1 e2 -> showNode (binopBuilder op) [ exprBuilder' e1
                                                         , exprBuilder' e2
                                                         ]
            Mem e -> showNode "mem" [exprBuilder' e]
            Call fun args -> showNode (labelBuilder fun <> "()") $ map exprBuilder' args
            ESeq stmt expr -> showNode ";" [stmtBuilder' stmt, exprBuilder' expr]

        labelBuilder' :: Temp.Label -> Builder -> Builder -> Builder
        labelBuilder' = showLeaf . labelBuilder

        binopBuilder :: Binop -> Builder
        binopBuilder = \case
            Add    -> "+"
            Sub    -> "-"
            Mul    -> "*"
            Div    -> "/"
            And    -> "&"
            Or     -> "|"
            Xor    -> "^"
            LShift -> "<<"
            RShift -> ">>"

        relopBuilder :: Relop -> Builder
        relopBuilder = \case
            Eq -> "="
            Ne -> "<>"
            Lt -> "<"
            Le -> "<="
            Gt -> ">"
            Ge -> ">="
