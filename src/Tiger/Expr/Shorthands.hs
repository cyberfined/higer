module Tiger.Expr.Shorthands where

import Data.Text
import Data.Fix
import Tiger.Expr.Types

-- Expr Shorthands

mkIntLit :: Int -> Expr d
mkIntLit = Fix . IntLit

mkStrLit :: Text -> Expr d
mkStrLit = Fix . StrLit

mkNil :: Expr d
mkNil = Fix Nil

mkLVal :: LValF d (Expr d) -> Expr d
mkLVal = Fix . LVal

mkDot :: LValF d (Expr d) -> Text -> Int -> LValF d (Expr d)
mkDot lv dot off = Dot (mkLVal lv) dot off

mkIndex :: LValF d (Expr d) -> Expr d -> LValF d (Expr d)
mkIndex lv ind = Index (mkLVal lv) (unFix ind)

mkNeg :: Expr d -> Expr d
mkNeg = Fix . Neg

mkBinop :: Binop -> Expr d -> Expr d -> Expr d
mkBinop op e1 e2 = Fix (Binop e1 op e2)

mkRecord :: Text -> [(Text, Expr d)] -> Expr d
mkRecord tid fs = Fix (Record tid fs)

mkArray :: Text -> Expr d -> Expr d -> Expr d
mkArray tid sz ini = Fix (Array tid sz ini)

mkCall :: Text -> [Expr d] -> Expr d
mkCall fn args = Fix (Call fn args)

mkAssign :: LValF d (Expr d) -> Expr d -> Expr d
mkAssign lv rv = Fix (Assign lv rv)

mkIf :: [Expr d] -> Expr d
mkIf [cond, th, el] = Fix (If cond th (Just el))
mkIf [cond, th] = Fix (If cond th Nothing)
mkIf _ = error "mkIf takes 2 or 3 arguments"

mkWhile :: Expr d -> Expr d -> Expr d
mkWhile cond body = Fix (While cond body)

mkFor :: Text -> Bool -> Expr d -> Expr d -> Expr d -> Expr d
mkFor var esc from to body = Fix (For var esc from to body)

mkBreak :: Expr d
mkBreak = Fix Break

mkLet :: [d (Expr d)] -> Expr d -> Expr d
mkLet decs expr = Fix (Let decs expr)

mkSeq :: [Expr d] -> Expr d
mkSeq = Fix . Seq

($+), ($-), ($*), ($/), ($>), ($>=), ($<), ($<=), ($!=), ($==), ($&&), ($||)
    :: Expr d -> Expr d -> Expr d
($+)  = mkBinop Add
($-)  = mkBinop Sub
($*)  = mkBinop Mul
($/)  = mkBinop Div
($>)  = mkBinop Gt
($>=) = mkBinop Ge
($<)  = mkBinop Lt
($<=) = mkBinop Le
($==) = mkBinop Eq
($!=) = mkBinop Ne
($&&) = mkBinop And
($||) = mkBinop Or
