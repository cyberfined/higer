{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}

module Tiger.Expr.Types
  ( Type(..)
  , Expr
  , ExprF(..)
  , Escaping(..)
  , RecordField(..)
  , LVal
  , LValF(..)
  , Binop(..)
  , UntypedDec(..)
  , UntypedType(..)
  , UntypedVar(..)
  , UntypedFun(..)
  , TypedDec(..)
  , TypedType(..)
  , TypedVar(..)
  , TypedFun(..)
  , TypeRVal(..)
  , UntypedFunArg(..)
  , TypedFunArg(..)

  , showType
  , showExpr
  , showBinop
  , showTypeRVal
  ) where

import Data.Text (Text, cons, snoc, intercalate, null)
import Data.Functor.Classes (Eq1(..))
import Prelude hiding (pred, null)

import Tiger.Utils
import Data.Fix

newtype PrefixFunc = PrefixFunc {apPrefixFunc :: Text -> Text -> Text}

class ShowPrefixed s where
    showPrefixed :: s -> PrefixFunc

instance ShowPrefixed PrefixFunc where
    showPrefixed = id

data RecordField a = RecordField
    { rfName  :: Text
    , rfValue :: a
    } deriving (Eq, Functor, Foldable, Traversable)

instance Eq1 RecordField where
    liftEq pred (RecordField n1 v1) (RecordField n2 v2) =  n1 == n2
                                                        && pred v1 v2

data Type
    = TInt
    | TString
    | TRecord Text [RecordField Type]
    | TArray Type
    | TFunc [Type] Type
    | TNil
    | TUnit
    | TName Text
    | TSelf
    deriving Eq

showType :: Type -> Text
showType = \case
    TInt -> "int"
    TString -> "string"
    TRecord r fs -> r <> " { " <> showRecordFields (map (fmap showType) fs) <> " }"
    TArray t -> "array of " <> showType t
    TFunc args ret -> intercalate " -> " (map showType args) <> " -> " <> showType ret
    TNil -> "nil"
    TUnit -> "()"
    TName s -> cons '\"' (snoc s '\"')
    TSelf -> "self"

-- Does variable escape current scope?
data Escaping
    = Escaping
    | Remaining
    deriving Eq

data ExprF d e
    = IntLit Int
    | StrLit Text
    | Nil
    | LVal (LValF d e)
    | Neg e
    | Binop e Binop e
    | Record Text [RecordField e]
    | Array Text e e
    | Call Text [e]
    | Assign (LValF d e) e
    | If e e (Maybe e)
    | While e e
    | For Text Escaping e e e
    | Break
    | Let [d e] e
    | Seq [e]
    deriving (Functor, Foldable, Traversable)

type Expr d = Fix (ExprF d)

instance Eq1 d => Eq1 (ExprF d) where
    liftEq pred a b = case (a,b) of
        (IntLit i1, IntLit i2)                 -> i1 == i2
        (StrLit s1, StrLit s2)                 -> s1 == s2
        (Nil, Nil)                             -> True
        (LVal lv1, LVal lv2)                   -> liftEq pred lv1 lv2
        (Neg e1, Neg e2)                       -> pred e1 e2
        (Binop e11 op1 e21, Binop e12 op2 e22) -> op1 == op2
                                               && pred e11 e12
                                               && pred e21 e22
        (Record tid1 fs1, Record tid2 fs2)     -> tid1 == tid2
                                               && liftEq (liftEq pred) fs1 fs2
        (Array tid1 sz1 ini1,
            Array tid2 sz2 ini2)               -> tid1 == tid2
                                               && pred sz1 sz2
                                               && pred ini1 ini2
        (Call fn1 args1, Call fn2 args2)       -> fn1 == fn2
                                               && liftEq pred args1 args2
        (Assign lv1 rv1, Assign lv2 rv2)       -> liftEq pred lv1 lv2
                                               && pred rv1 rv2
        (If cond1 th1 mel1, If cond2 th2 mel2) -> pred cond1 cond2
                                               && pred th1 th2
                                               && liftEq pred mel1 mel2
        (While cond1 body1, While cond2 body2) -> pred cond1 cond2
                                               && pred body1 body2
        (For v1 e1 from1 to1 body1,
            For v2 e2 from2 to2 body2)         -> v1 == v2
                                               && e1 == e2
                                               && pred from1 from2
                                               && pred to1 to2
                                               && pred body1 body2
        (Break, Break)                         -> True
        (Let decs1 e1, Let decs2 e2)           -> liftEq (liftEq pred) decs1 decs2
                                               && pred e1 e2
        (Seq es1, Seq es2)                     -> liftEq pred es1 es2
        _                                      -> False

showExpr :: (Functor d, ShowPrefixed (d PrefixFunc)) => Expr d -> Text
showExpr expr = apPrefixFunc (foldFix showExprF expr) "" ""

showExprF :: (Functor d, ShowPrefixed (d PrefixFunc)) => ExprF d PrefixFunc -> PrefixFunc
showExprF = \case
    IntLit i         -> showLeaf (showText i)
    StrLit s         -> showLeaf (showText s)
    Nil              -> showLeaf "nil"
    LVal lv          -> showLValF lv
    Neg e            -> showNode "-" [e]
    Binop e1 op e2   -> showNode (showBinop op) [e1, e2]
    Record r fs      -> showNodeNames r (map recordFieldToTuple fs)
    Array typ sz ini -> showNodeNames ("array of " <> typ)
        [ ("size", sz)
        , ("init", ini)
        ]
    Call fn args -> showNode (fn <> "()") args
    Assign lv rv -> showNode ":="
        [ showLValF lv
        , rv
        ]
    If cond th mel -> showNodeNames "if" $
        maybe [ ("", cond)
              , ("then", th)
              ]
              (\el -> [ ("", cond)
                      , ("then", th)
                      , ("else", el)
                      ]
              )
              mel
    While cond body -> showNodeNames "while"
        [ ("", cond)
        , ("do",body)
        ]
    For var esc from to body -> showNodeNames "for"
        [ ("", showLeaf $ "var " <> var <> showEsc esc)
        , ("from", from)
        , ("to", to)
        , ("do", body)
        ]
    Break -> showLeaf "break"
    Let decs expr -> showNodeNames "let" $
        foldr (\d ds -> ("",showPrefixed d):ds) [("in", expr)] decs
    Seq es -> showNode ";" es

data LValF d l
    = Var Text
    | Dot l Text Int
    | Index l (ExprF d l)
    deriving (Functor, Foldable, Traversable)

type LVal d = Fix (LValF d)

instance Eq1 d => Eq1 (LValF d) where
    liftEq pred a b = case (a,b) of
        (Var v1, Var v2)             -> v1 == v2
        (Dot l1 d1 o1, Dot l2 d2 o2) -> pred l1 l2
                                     && d1 == d2
                                     && o1 == o2
        (Index l1 i1, Index l2 i2)   -> pred l1 l2
                                     && liftEq pred i1 i2
        _                            -> False

showLValF :: (Functor d, ShowPrefixed (d PrefixFunc)) => LValF d PrefixFunc -> PrefixFunc
showLValF = \case
    Var v     -> showLeaf v
    Dot x y _ -> showNode "." [x, showLeaf y]
    Index x y -> showNode "[]" [x, showExprF y]

data Binop = Add | Sub | Mul | Div
           | Lt | Le | Gt | Ge
           | Eq | Ne | And | Or
           deriving Eq

showBinop :: Binop -> Text
showBinop = \case
    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    Div -> "/"
    Lt  -> "<"
    Le  -> "<="
    Gt  -> ">"
    Ge  -> ">="
    Eq  -> "="
    Ne  -> "<>"
    And -> "and"
    Or  -> "or"

data UntypedDec e
    = UntypedTypeDec UntypedType
    | UntypedVarDec (UntypedVar e)
    | UntypedFunDec (UntypedFun e)
    deriving (Functor, Foldable, Traversable)

data UntypedType = UntypedType
    { uTypeName :: Text
    , uTypeRVal :: TypeRVal
    } deriving Eq

data UntypedVar e = UntypedVar
    { uVarName  :: Text
    , uVarType  :: Maybe Text
    , uVarValue :: e
    } deriving (Functor, Foldable, Traversable)

data UntypedFun e = UntypedFun
    { uFunName :: Text
    , uFunArgs :: [UntypedFunArg]
    , uFunType :: Maybe Text
    , uFunBody :: e
    } deriving (Functor, Foldable, Traversable)

data UntypedFunArg = UntypedFunArg
    { uFunArgName :: Text
    , uFunArgType :: Text
    } deriving Eq

instance Eq1 UntypedDec where
    liftEq pred a b = case (a,b) of
        (UntypedTypeDec t1, UntypedTypeDec t2) -> t1 == t2
        (UntypedVarDec v1, UntypedVarDec v2)   -> liftEq pred v1 v2
        (UntypedFunDec f1, UntypedFunDec f2)   -> liftEq pred f1 f2
        _                                      -> False

instance Eq1 UntypedVar where
    liftEq pred a b = case (a,b) of
        (UntypedVar var1 typ1 val1, UntypedVar var2 typ2 val2) -> var1 == var2
                                                               && typ1 == typ2
                                                               && pred val1 val2

instance Eq1 UntypedFun where
    liftEq pred a b = case (a,b) of
        (UntypedFun fn1 args1 typ1 body1,
            UntypedFun fn2 args2 typ2 body2) -> fn1 == fn2
                                             && args1 == args2
                                             && typ1 == typ2
                                             && pred body1 body2

instance ShowPrefixed e => ShowPrefixed (UntypedDec e) where
    showPrefixed = \case
        UntypedTypeDec typ -> showPrefixed typ
        UntypedVarDec var  -> showPrefixed var
        UntypedFunDec fun  -> showPrefixed fun

instance ShowPrefixed UntypedType where
    showPrefixed (UntypedType typ rv) =
        showLeaf ("type " <> typ <> " = " <> showTypeRVal rv)

instance ShowPrefixed e => ShowPrefixed (UntypedVar e) where
    showPrefixed (UntypedVar var mtyp val) = case mtyp of
        Nothing -> showNode ":="
            [ showLeaf $ "var " <> var
            , showPrefixed val
            ]
        Just typ -> showNode ":="
            [ showLeaf $ "var " <> var <> ": " <> typ
            , showPrefixed val
            ]

instance ShowPrefixed e => ShowPrefixed (UntypedFun e) where
    showPrefixed (UntypedFun fun args mtyp body) = case mtyp of
        Nothing -> showNode ":="
            [ showLeaf $ "function " <> fun <> "( " <> showUntypedFunArgs args <> " )"
            , showPrefixed body
            ]
        Just typ -> showNode ":="
            [ showLeaf $  "function " <> fun
                       <> "( " <> showUntypedFunArgs args <> " ): "
                       <> typ
            , showPrefixed body
            ]

data TypedDec e
    = TypedTypeDec TypedType
    | TypedVarDec (TypedVar e)
    | TypedFunDec (TypedFun e)
    deriving (Functor, Foldable, Traversable)

data TypedType = TypedType
    { tTypeName :: Text
    , tTypeType :: Type
    } deriving Eq

data TypedVar e = TypedVar
    { tVarName     :: Text
    , tVarType     :: Type
    , tVarEscaping :: Escaping
    , tVarValue    :: e
    } deriving (Functor, Foldable, Traversable)

data TypedFun e = TypedFun
    { tFunName :: Text
    , tFunArgs :: [TypedFunArg]
    , tFunType :: Type
    , tFunBody :: e
    } deriving (Functor, Foldable, Traversable)

data TypedFunArg = TypedFunArg
    { tFunArgName     :: Text
    , tFunArgType     :: Type
    , tFunArgEscaping :: Escaping
    } deriving Eq

instance Eq1 TypedDec where
    liftEq pred a b = case (a,b) of
        (TypedTypeDec t1, TypedTypeDec t2) -> t1 == t2
        (TypedVarDec v1, TypedVarDec v2)   -> liftEq pred v1 v2
        (TypedFunDec f1, TypedFunDec f2)   -> liftEq pred f1 f2
        _                                  -> False

instance Eq1 TypedVar where
    liftEq pred a b = case (a,b) of
        (TypedVar v1 t1 esc1 e1, TypedVar v2 t2 esc2 e2) -> v1 == v2
                                                         && t1 == t2
                                                         && esc1 == esc2
                                                         && pred e1 e2

instance Eq1 TypedFun where
    liftEq pred a b = case (a,b) of
        (TypedFun f1 args1 t1 b1, TypedFun f2 args2 t2 b2) -> f1 == f2
                                                           && args1 == args2
                                                           && t1 == t2
                                                           && pred b1 b2

instance ShowPrefixed e => ShowPrefixed (TypedDec e) where
    showPrefixed = \case
        TypedTypeDec typ -> showPrefixed typ
        TypedVarDec var  -> showPrefixed var
        TypedFunDec fun  -> showPrefixed fun

instance ShowPrefixed TypedType where
    showPrefixed (TypedType tid typ) = showLeaf ("type " <> tid <> " = " <> showType typ)

instance ShowPrefixed e => ShowPrefixed (TypedVar e) where
    showPrefixed (TypedVar var typ esc val) = showNode ":="
        [ showLeaf $  "var " <> var
                   <> ": " <> showType typ
                   <> showEsc esc
        , showPrefixed val
        ]

instance ShowPrefixed e => ShowPrefixed (TypedFun e) where
    showPrefixed (TypedFun fun args typ body) = showNode ":="
        [ showLeaf $  "function " <> fun
                   <> "( " <> showTypedFunArgs args <> " ): "
                   <> showType typ
        , showPrefixed body
        ]

data TypeRVal
    = TypeId Text
    | TypeRecord [RecordField Text]
    | TypeArray Text
    deriving Eq

showTypeRVal :: TypeRVal -> Text
showTypeRVal = \case
    TypeId t      -> t
    TypeRecord xs -> "{ " <> showRecordFields xs <> " }"
    TypeArray t   -> "array of " <> t

showNodeNames :: Text -> [(Text, PrefixFunc)] -> PrefixFunc
showNodeNames node xs = PrefixFunc (\pr cpr -> pr <> node <> cons '\n' (showXs pr cpr))
  where showXs :: Text -> Text -> Text
        showXs = snd (foldr (\a b -> (False, go a b)) (True, \_ _ -> "") xs)

        go :: (Text, PrefixFunc) -> (Bool, Text -> Text -> Text) -> Text -> Text -> Text
        go (name, PrefixFunc pfunc) (isLast, fs) pr cpr
          | null name = pfunc newPr newCpr <> rest
          | otherwise = showName <> rest
          where rest = fs pr cpr
                (newPr, newCpr) = if isLast
                                     then (cpr <> "└── ", cpr <> "    ")
                                     else (cpr <> "├── ", cpr <> "│   ")
                showName =  newPr <> name
                         <> cons '\n' (pfunc (newCpr <> "└── ") (newCpr <> "    "))

showNode :: Text -> [PrefixFunc] -> PrefixFunc
showNode node = showNodeNames node . map (\x -> ("",x))

showLeaf :: Text -> PrefixFunc
showLeaf s = PrefixFunc (\pr _ -> pr <> s <> "\n")

showEsc :: Escaping -> Text
showEsc Escaping  = " [escaping]"
showEsc Remaining = ""

showRecordFields :: [RecordField Text] -> Text
showRecordFields = intercalate ", " . map (\rf -> rfName rf <> ": " <> rfValue rf)

recordFieldToTuple :: RecordField e -> (Text, e)
recordFieldToTuple rf = (rfName rf, rfValue rf)

showUntypedFunArgs :: [UntypedFunArg] -> Text
showUntypedFunArgs = intercalate ", " . map showUntypedFunArg
  where showUntypedFunArg :: UntypedFunArg -> Text
        showUntypedFunArg ufa = uFunArgName ufa <> ": " <> uFunArgType ufa

showTypedFunArgs :: [TypedFunArg] -> Text
showTypedFunArgs = intercalate ", " .  map showTypedFunArg
  where showTypedFunArg :: TypedFunArg -> Text
        showTypedFunArg tfa =  tFunArgName tfa
                            <> ": "
                            <> showType (tFunArgType tfa)
                            <> showEsc (tFunArgEscaping tfa)
