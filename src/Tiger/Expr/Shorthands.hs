module Tiger.Expr.Shorthands
    ( mkIntLit
    , mkStrLit
    , mkNil
    , mkLVal
    , mkDot
    , mkIndex
    , mkNeg
    , mkBinop
    , mkRecord
    , mkArray
    , mkCall
    , mkAssign
    , mkIf
    , mkWhile
    , mkFor
    , mkBreak
    , mkLet
    , mkSeq
    , mkUntypedType
    , mkUntypedVar
    , mkUntypedFun
    , mkTypedType
    , mkTypedVar
    , mkTypedFun
    , ($+), ($-), ($*), ($/)
    , ($<), ($<=), ($>), ($>=)
    , ($==), ($!=), ($&&), ($||)
    ) where

import Data.Fix
import Data.Text
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

mkRecord :: Text -> [RecordField (Expr d)] -> Expr d
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

mkFor :: Text -> Escaping -> Expr d -> Expr d -> Expr d -> Expr d
mkFor var esc from to body = Fix (For var esc from to body)

mkBreak :: Expr d
mkBreak = Fix Break

mkLet :: [d (Expr d)] -> Expr d -> Expr d
mkLet decs expr = Fix (Let decs expr)

mkSeq :: [Expr d] -> Expr d
mkSeq = Fix . Seq

mkUntypedType :: Text -> TypeRVal -> UntypedDec a
mkUntypedType tid typ = UntypedTypeDec (UntypedType tid typ)

mkUntypedVar :: Text -> Maybe Text -> a -> UntypedDec a
mkUntypedVar var mtyp val = UntypedVarDec (UntypedVar var mtyp val)

mkUntypedFun :: Text -> [UntypedFunArg] -> Maybe Text -> a -> UntypedDec a
mkUntypedFun fun args mtyp body = UntypedFunDec (UntypedFun fun args mtyp body)

mkTypedType :: Text -> Type -> TypedDec a
mkTypedType tid typ = TypedTypeDec (TypedType tid typ)

mkTypedVar :: Text -> Type -> Escaping -> e -> TypedDec e
mkTypedVar var typ esc val = TypedVarDec (TypedVar var typ esc val)

mkTypedFun :: Text -> [TypedFunArg] -> Type -> e -> TypedDec e
mkTypedFun fun args typ body = TypedFunDec (TypedFun fun args typ body)

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
