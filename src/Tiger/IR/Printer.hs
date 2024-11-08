module Tiger.IR.Printer () where

import           Data.Text.Lazy.Builder     (Builder)

import           Tiger.Frame
import           Tiger.IR.Types
import           Tiger.TextUtils

import qualified Data.Graph.Inductive       as Graph
import qualified Data.List.NonEmpty         as NonEmpty
import qualified Data.Text.Lazy.Builder.Int as Builder

import qualified Tiger.Temp                 as Temp

instance (TextBuildable b, Frame f) => TextBuildable (IRData b f) where
    toTextBuilder IRData{..} = strings <> funcs
      where strings =
                let res = foldMap (\str -> toTextBuilder str <> "\n") irStrings
                in if null irStrings then "" else "(strings)\n" <> res <> "\n"

            funcs =  "(functions)\n"
                  <> intercalate "\n\n" (map toTextBuilder irFunctions)

instance (TextBuildable b, Frame f) => TextBuildable (IRFunction b f) where
    toTextBuilder IRFunction{..} = signature <> toTextBuilder irFuncBody
      where signature =
                let args = intercalate ", " (map toTextBuilder $ frameArgs irFuncFrame)
                in toTextBuilder (frameName irFuncFrame)
                <> "(" <> args <> "):\n"
                <> toTextBuilder irFuncFrame <> "\n"

instance TextBuildable s => TextBuildable (ControlFlowGraph s) where
    toTextBuilder = cfgBuilder . cfgGraph
      where cfgBuilder = intercalate "\n\n" . Graph.dfsWith' nodeBuilder
            nodeBuilder (_, _, Block{..}, _) =
                intercalate "\n" (map toTextBuilder blockStmts)

instance TextBuildable Stmt where
    toTextBuilder s = stmtBuilder' s "" ""
      where stmtBuilder' :: Stmt -> Builder -> Builder -> Builder
            stmtBuilder' = \case
                Move dst src -> showNode "move" [exprBuilder' dst, exprBuilder' src]
                Expr e -> exprBuilder' e
                Jump l -> showLeaf $ "jump " <> toTextBuilder l
                CJump op e1 e2 tLab fLab ->
                    showNodeNames ("cjump " <> relopBuilder op)
                        [ (Nothing, exprBuilder' e1)
                        , (Nothing, exprBuilder' e2)
                        , (Just "true", labelBuilder' tLab)
                        , (Just "false", labelBuilder' fLab)
                        ]
                Seq es -> showNode "seq" $ map stmtBuilder' $ NonEmpty.toList es
                Label l -> showLeaf $ toTextBuilder l <> ":"
                Ret -> showLeaf "ret"

            exprBuilder' :: Expr -> Builder -> Builder -> Builder
            exprBuilder' = \case
                Const i -> showLeaf $ Builder.decimal i
                Name l  -> showLeaf $ toTextBuilder l
                Temp t  -> showLeaf $ toTextBuilder t
                Binop op e1 e2 -> showNode (binopBuilder op) [ exprBuilder' e1
                                                             , exprBuilder' e2
                                                             ]
                Mem e -> showNode "mem" [exprBuilder' e]
                Call fun args -> showNode (toTextBuilder fun <> "()") $
                    map exprBuilder' args
                ESeq stmt expr -> showNode "eseq" [stmtBuilder' stmt, exprBuilder' expr]

            labelBuilder' :: Temp.Label -> Builder -> Builder -> Builder
            labelBuilder' = showLeaf . toTextBuilder

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

instance TextBuildable LabeledString where
    toTextBuilder LabeledString{..} = toTextBuilder lStringLabel
                                   <> ": "
                                   <> stringBuilder lStringValue
