module Absyn (
    Type(..),
    Posable(..),
    SourcePos(..),
    Dec(..),
    TypeDec(..),
    VarDec(..),
    FunDec(..),
    TypeRval(..),
    LVal(..),
    Expr(..),
    BinOp(..)
    ) where

import Data.List(intercalate)

data Type = TInt
          | TString
          | TRecord String [(String, Type)]
          | TArray Type
          | TFunc [Type] Type
          | TNil
          | TUnit
          | TName String
          | TRecursive
          deriving Eq

instance Show Type where
    show t = case t of
        TInt -> "int"
        TString -> "string"
        TArray t -> "array of " ++ show t
        TRecord r xs -> r ++ " { " ++ typedFields xs ++ " }"
        TFunc ts res -> intercalate " -> " (map show (res:ts))
        TNil -> "nil"
        TUnit -> "()"
        TName s -> s
        TRecursive -> "recursive"

data SourcePos = SourcePos String Int Int deriving (Eq, Show)

class Posable p where
    position :: p -> SourcePos

instance Posable SourcePos where
    position = id

data Dec = TypeDec SourcePos TypeDec
         | VarDec SourcePos VarDec
         | FunDec SourcePos FunDec
         deriving Eq

instance Posable Dec where
    position p = case p of
        TypeDec sc _ -> sc
        VarDec sc _ -> sc
        FunDec sc _ -> sc

instance Show Dec where
    show = showdec "" ""

showdec pr cpr dec = case dec of
    TypeDec _ td -> case td of
        UntypedType tid rv -> pr ++ "type " ++ tid ++ " = " ++ show rv ++ "\n"
        TypedType tid t -> pr ++ "type " ++ tid ++ " = " ++ show t ++ "\n"
    VarDec _ vd -> case vd of
        UntypedVar v mt exp -> case mt of
            Nothing -> pr ++ "=\n" ++ cpr ++ "├── var " ++ v ++ '\n':showexpr (cpr ++ "└── ") (cpr ++ "    ") exp
            Just t -> pr ++ "=\n" ++ cpr ++ "├── var " ++ v ++ ": " ++ t ++ '\n':showexpr (cpr ++ "└── ") (cpr ++ "    ") exp
        TypedVar v t esc exp -> pr ++ "=\n" ++ cpr ++ "├── var " ++ v ++ ": " ++ show t ++ (if esc then " [escaping]" else "") ++ '\n':showexpr (cpr ++ "└── ") (cpr ++ "    ") exp
    FunDec _ fd -> case fd of
        UntypedFun f xs mt exp -> case mt of
            Nothing -> pr ++ "=\n" ++ cpr ++ "├── function " ++ f ++ "( " ++ fields xs ++ " )\n" ++ showexpr (cpr ++ "└── ") (cpr ++ "    ") exp
            Just t -> pr ++ "=\n" ++ cpr ++ "├── function " ++ f ++ "( " ++ fields xs ++ " ): " ++ t ++ '\n':showexpr (cpr ++ "└── ") (cpr ++ "    ") exp
        TypedFun f xs t exp -> pr ++ "=\n" ++ cpr ++ "├── function " ++ f ++ "( " ++ escTypedFields xs ++ " ): " ++ show t ++ '\n':showexpr (cpr ++ "└── ") (cpr ++ "    ") exp
          where escTypedFields = intercalate ", " . map (\(x,t,e) -> x ++ ": " ++ show t ++ (if e then " [escaping]" else ""))

fields :: [(String, String)] -> String
fields = intercalate ", " . map (\(x,t) -> x ++ ": " ++ t)

typedFields :: [(String, Type)] -> String
typedFields = intercalate ", " . map (\(x,t) -> x ++ ": " ++ show t)

data TypeDec = UntypedType String TypeRval
             | TypedType String Type
             deriving Eq

data VarDec = UntypedVar String (Maybe String) Expr
            | TypedVar String Type Bool Expr
            deriving Eq

data FunDec = UntypedFun String [(String, String)] (Maybe String) Expr
            | TypedFun String [(String, Type, Bool)] Type Expr
            deriving Eq

data TypeRval = TypeId String
              | TypeRecord [(String, String)]
              | TypeArray String
              deriving Eq

instance Show TypeRval where
    show rv = case rv of
        TypeId t -> t
        TypeRecord xs -> "{ " ++ fields xs ++ " }"
        TypeArray t -> "array of " ++ t

data LVal = LId String
          | LDot LVal String
          | LArr LVal Expr
          deriving Eq

instance Show LVal where
    show = showlval "" ""

showlval :: String -> String -> LVal -> String
showlval pr cpr lv = case lv of
    LId x -> pr ++ x ++ "\n"
    LDot x y -> pr ++ ".\n" ++ showlval (cpr ++ "├── " ) (cpr ++ "│   ") x ++ cpr ++ "└── " ++ y ++ "\n"
    LArr x y -> pr ++ "[]\n" ++ showlval (cpr ++ "├── " ) (cpr ++ "│   ") x ++ showexpr (cpr ++ "└── ") (cpr ++ "    ") y

data Expr = LVal SourcePos LVal
          | Nil SourcePos
          | Seq SourcePos [Expr]
          | IntLit SourcePos Int
          | StrLit SourcePos String
          | Neg SourcePos Expr
          | Call SourcePos String [Expr]
          | BinOp SourcePos Expr BinOp Expr
          | Record SourcePos String [(String, Expr)]
          | Array SourcePos String Expr Expr
          | Assign SourcePos LVal Expr
          | If SourcePos Expr Expr (Maybe Expr)
          | While SourcePos Expr Expr
          | For SourcePos String Bool Expr Expr Expr
          | Break SourcePos
          | Let SourcePos [Dec] Expr
          deriving Eq

instance Posable Expr where
    position p = case p of
        LVal sc _ -> sc
        Nil sc -> sc
        Seq sc _ -> sc
        IntLit sc _ -> sc
        StrLit sc _ -> sc
        Neg sc _ -> sc
        Call sc _ _ -> sc
        BinOp sc _ _ _ -> sc
        Record sc _ _ -> sc
        Array sc _ _ _ -> sc
        Assign sc _ _ -> sc
        If sc _ _ _ -> sc
        While sc _ _ -> sc
        For sc _ _ _ _ _ -> sc
        Break sc -> sc
        Let sc _ _ -> sc

instance Show Expr where
    show = showexpr "" ""

showexpr :: String -> String -> Expr -> String
showexpr pr cpr e = case e of
    LVal _ lv -> showlval pr cpr lv
    Nil _ -> pr ++ "nil\n"
    Seq _ xs -> pr ++ ";\n" ++ showList xs
    IntLit _ i -> pr ++ show i ++ "\n"
    StrLit _ s -> pr ++ show s ++ "\n"
    Neg _ x -> pr ++ "-\n" ++ showList [x]
    Call _ n vs -> pr ++ n++"()\n" ++ showList vs
    BinOp _ e1 op e2 -> pr ++ show op ++ '\n':showList [e1, e2]
    Record _ tid fs -> pr ++ tid ++ '\n':showFields fs
    Array _ n s v -> pr ++ "array: " ++ n ++ "\n" ++ cpr ++ "├── size\n" ++ showsub "│   " s ++ cpr ++ "└── init\n" ++ showsub "    " v
    Assign _ x y -> pr ++ ":=\n" ++ showList [LVal undefined x, y]
    If _ cond th el ->
        pr ++ "if\n" ++ case el of
                  Nothing -> showexpr (cpr ++ "├── ") (cpr ++ "│   ") cond ++ cpr ++ "└── then\n" ++ showsub "    " th
                  Just el -> showexpr (cpr ++ "├── ") (cpr ++ "│   ") cond ++ cpr ++ "├── then\n" ++ showsub "│   " th ++ cpr ++ "└── else\n" ++ showsub "    " el
    While _ cond lp -> pr ++ "while\n" ++ showexpr (cpr ++ "├── ") (cpr ++ "│   ") cond ++ cpr ++ "└── do\n" ++ showsub "    " lp
    For _ i esc e1 e2 e3 ->
        pr ++ "for" ++ '\n':showdec (cpr ++ "├── ") (cpr ++ "│   ") (VarDec undefined (TypedVar i TInt esc e1)) ++ cpr ++ "├── to\n" ++ showsub "│   " e2 ++ cpr ++ "└── do\n" ++ showsub "    " e3
    Break _ -> pr ++ "break\n"
    Let _ ds exp -> pr ++ "let\n" ++ showlet ds exp
  where showList (x:xs@(_:_)) = showexpr (cpr ++ "├── " ) (cpr ++ "│   ") x ++ showList xs
        showList [x] = showexpr (cpr ++ "└── ") (cpr ++ "    ") x
        showList _ = ""
        showsub ncpr x = showexpr (cpr ++ ncpr ++ "└── ") (cpr ++ ncpr ++ "    ") x
        showFields ((x, e):xs@(_:_)) = cpr ++ "├── " ++ x ++ "\n" ++ showsub "│   " e ++ showFields xs
        showFields [(x, e)] = cpr ++ "└── " ++ x ++ "\n" ++ showsub "    " e
        showlet (x:xs) e = showdec (cpr ++ "├── ") (cpr ++ "│   ") x ++ showlet xs e
        showlet _ e = cpr ++ "└── in\n" ++ showexpr (cpr ++ "    └── ") (cpr ++ "        ") e

data BinOp = Add
           | Sub
           | Mul
           | Div
           | Lt
           | Le
           | Gt
           | Ge
           | Eq
           | Ne
           | And
           | Or
           deriving Eq

instance Show BinOp where
    show op = case op of
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
        And -> "&"
        Or -> "|"
