{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE OverloadedStrings   #-}

module Tiger.Expr.Types
  ( Type(..)
  , Expr
  , ExprF(..)
  , LVal
  , LValF(..)
  , Binop(..)
  , UntypedDec(..)
  , TypedDec(..)
  , TypeRVal(..)

  , showType
  , showExpr
  , showBinop
  , showTypeRVal
  )
where

import Data.Text hiding(map, head, null, foldr, zipWith, length)
import qualified Data.Text as T
import Data.Functor.Classes(Eq1(..))

import Tiger.Utils
import Data.Fix

newtype PFunc = PFunc {unPF :: Text -> Text -> Text}

class ShowPrefixed s where
    showPrefixed :: s -> PFunc

instance ShowPrefixed PFunc where
    showPrefixed = id

data Type = TInt
          | TString
          | TRecord Text [(Text, Type)]
          | TArray Type
          | TFunc [Type] Type
          | TNil
          | TUnit
          | TName Text
          | TSelf
          deriving (Eq, Show)

showType :: Type -> Text
showType typ = case typ of
    TInt -> "int"
    TString -> "string"
    TRecord r fs -> r <> " { " <> fields (map (fmap showType) fs) <> " }"
    TArray t -> "array of " <> showType t
    TFunc args ret -> intercalate " -> " (map showType args) <> " -> " <> showType ret
    TNil -> "nil"
    TUnit -> "()"
    TName s -> cons '\"' (snoc s '\"')
    TSelf -> "self"

data ExprF d e = IntLit Int
               | StrLit Text
               | Nil
               | LVal (LValF d e)
               | Neg e
               | Binop e Binop e
               | Record Text [(Text, e)]
               | Array Text e e
               | Call Text [e]
               | Assign (LValF d e) e
               | If e e (Maybe e)
               | While e e
               | For Text Bool e e e
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
        (Record tid1 fs1, Record tid2 fs2)     -> let (f1, e1) = unzip fs1
                                                      (f2, e2) = unzip fs2
                                                  in tid1 == tid2
                                                  && f1 == f2
                                                  && liftEq pred e1 e2
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

showExpr :: (Functor d, ShowPrefixed (d PFunc)) => Expr d -> Text
showExpr expr = unPF (foldFix showExprF expr) "" ""

showExprF :: (Functor d, ShowPrefixed (d PFunc)) => ExprF d PFunc -> PFunc
showExprF expr = case expr of
    IntLit i -> showLeaf (showText i)
    StrLit s -> showLeaf (showText s)
    Nil -> showLeaf "nil"
    LVal lv -> showLValF lv
    Neg e -> showNode "-" [e]
    Binop e1 op e2 -> showNode (showBinop op) [e1, e2]
    Record r fs -> showNodeNames r fs
    Array typ sz ini -> showNodeNames ("array of " <> typ) [ ("size", sz)
                                                           , ("init", ini)
                                                           ]
    Call fn args -> showNode (fn <> "()") args
    Assign lv rv -> showNode ":=" [ showLValF lv
                                  , rv
                                  ]
    If cond th mel -> showNodeNames "if" $ maybe [ ("", cond)
                                                 , ("then", th)
                                                 ]
                                                 (\el -> [ ("", cond)
                                                         , ("then", th)
                                                         , ("else", el)
                                                         ]
                                                 )
                                                 mel
    While cond body -> showNodeNames "while" [ ("", cond)
                                             , ("do",body)
                                             ]
    For var esc from to body -> showNodeNames "for" [ ("", showLeaf $ "var " <> var <> showEsc esc)
                                                    , ("from", from)
                                                    , ("to", to)
                                                    , ("do", body)
                                                    ]
    Break -> showLeaf "break"
    Let decs expr -> showNodeNames "let" $
        foldr (\d ds -> ("",showPrefixed d):ds) [("in", expr)] decs
    Seq es -> showNode ";" es

data LValF d l = Var Text
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

showLValF :: (Functor d, ShowPrefixed (d PFunc)) => LValF d PFunc -> PFunc
showLValF lv = case lv of
    Var v -> showLeaf v
    Dot x y _ -> showNode "." [x, showLeaf y]
    Index x y -> showNode "[]" [x, showExprF y]

data Binop = Add | Sub | Mul | Div
           | Lt | Le | Gt | Ge
           | Eq | Ne | And | Or
           deriving Eq

showBinop :: Binop -> Text
showBinop op = case op of
    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    Div -> "/"
    Lt -> "<"
    Le -> "<="
    Gt -> ">"
    Ge -> ">="
    Eq -> "="
    Ne -> "<>"
    And -> "and"
    Or -> "or"

data UntypedDec e = UntypedType Text TypeRVal
                  | UntypedVar Text (Maybe Text) e
                  | UntypedFun Text [(Text, Text)] (Maybe Text) e
                  deriving (Functor, Foldable, Traversable)

instance Eq1 UntypedDec where
    liftEq pred a b = case (a,b) of
        (UntypedType tid1 rv1, UntypedType tid2 rv2) -> tid1 == tid2
                                                     && rv1 == rv2
        (UntypedVar v1 mt1 e1, UntypedVar v2 mt2 e2) -> v1 == v2
                                                     && mt1 == mt2
                                                     && pred e1 e2
        (UntypedFun f1 args1 mt1 e1,
            UntypedFun f2 args2 mt2 e2)              -> f1 == f2
                                                     && args1 == args2
                                                     && mt1 == mt2
                                                     && pred e1 e2
        _                                            -> False

instance ShowPrefixed e => ShowPrefixed (UntypedDec e) where
    showPrefixed dec = case dec of
        UntypedType tid rv -> showLeaf ("type " <> tid <> " = " <> showTypeRVal rv)
        UntypedVar v mt expr -> case mt of
            Nothing -> showNode ":=" [ showLeaf $ "var " <> v
                                     , showPrefixed expr
                                     ]
            Just t -> showNode ":=" [ showLeaf $ "var " <> v <> ": " <> t
                                    , showPrefixed expr
                                    ]
        UntypedFun f xs mt expr -> case mt of
            Nothing -> showNode ":=" [ showLeaf $ "function " <> f <> "( " <> fields xs <> " )"
                                     , showPrefixed expr
                                     ]
            Just t -> showNode ":=" [ showLeaf $  "function " <> f
                                               <> "( " <> fields xs <> " ): "
                                               <> t
                                    , showPrefixed expr
                                    ]

data TypedDec e = TypedType Text Type
                | TypedVar Text Type Bool e
                | TypedFun Text [(Text, Type, Bool)] Type e
                deriving (Functor, Foldable, Traversable)

instance Eq1 TypedDec where
    liftEq pred a b = case (a,b) of
        (TypedType tid1 t1, TypedType tid2 t2)             -> tid1 == tid2
                                                           && t1 == t2
        (TypedVar v1 t1 esc1 e1, TypedVar v2 t2 esc2 e2)   -> v1 == v2
                                                           && t1 == t2
                                                           && esc1 == esc2
                                                           && pred e1 e2
        (TypedFun f1 args1 t1 e1, TypedFun f2 args2 t2 e2) -> f1 == f2
                                                           && args1 == args2
                                                           && t1 == t2
                                                           && pred e1 e2
        _                                                  -> False

instance ShowPrefixed e => ShowPrefixed (TypedDec e) where
    showPrefixed dec = case dec of
        TypedType tid t -> showLeaf ("type " <> tid <> " = " <> showType t)
        TypedVar v t esc expr -> showNode ":=" [ showLeaf $  "var " <> v
                                                          <> ": " <> showType t
                                                          <> showEsc esc
                                               , showPrefixed expr
                                               ]
        TypedFun f xs t expr -> showNode ":=" [ showLeaf $  "function " <> f
                                                         <> "( " <> escFields xs <> " ): "
                                                         <> showType t
                                              , showPrefixed expr
                                              ]

data TypeRVal = TypeId Text
              | TypeRecord [(Text, Text)]
              | TypeArray Text
              deriving Eq

showTypeRVal :: TypeRVal -> Text
showTypeRVal rv = case rv of
    TypeId t -> t
    TypeRecord xs -> "{ " <> fields xs <> " }"
    TypeArray t -> "array of " <> t

showNodeNames :: Text -> [(Text, PFunc)] -> PFunc
showNodeNames name xs = PFunc (\pr cpr -> pr <> name <> cons '\n' (showXs pr cpr))
  where showXs = snd (foldr (\a b -> (False, go a b)) (True, \_ _ -> "") xs)
        go (n, PFunc f) (isLast, fs) pr cpr
          | T.null n = f newPr newCpr <> rest
          | otherwise = showName <> rest
          where rest = fs pr cpr
                (newPr, newCpr) = if isLast
                                     then (cpr <> "└── ", cpr <> "    ")
                                     else (cpr <> "├── ", cpr <> "│   ")
                showName = newPr <> n <> cons '\n' (f (newCpr <> "└── ") (newCpr <> "    "))

showNode :: Text -> [PFunc] -> PFunc
showNode name = showNodeNames name . map (\x -> ("",x))

showLeaf :: Text -> PFunc
showLeaf s = PFunc (\pr _ -> pr <> s <> "\n")

showEsc :: Bool -> Text
showEsc esc = if esc then " [escaping]" else ""

fields :: [(Text, Text)] -> Text
fields = intercalate ", " . map (\(x,t) -> x <> ": " <> t)

escFields :: [(Text, Type, Bool)] -> Text
escFields = intercalate ", " . map (\(x,t,e) -> x <> ": " <> showType t <> showEsc e)
