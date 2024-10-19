module Tiger.Expr
    ( Dec(..)
    , TypeDec(..)
    , TypeDecBody(..)
    , FunDec(..)
    , DecField(..)
    , RecordField(..)
    , Expr(..)
    , LVal(..)
    , Escaping(..)
    , Binop(..)
    , Field(..)
    , Span(..)
    , Position(..)

    , exprSpan
    , setExprSpan
    , lValSpan
    , setLValSpan
    ) where

import           Data.List.NonEmpty         (NonEmpty)
import           Data.Text                  (Text)
import           Data.Text.Lazy.Builder     (Builder)
import           Numeric.Natural            (Natural)
import           Prelude                    hiding (span)
import           Tiger.TextUtils

import qualified Data.List.NonEmpty         as NonEmpty
import qualified Data.Text.Lazy.Builder     as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder

data Dec
    = TypeDecs !(NonEmpty TypeDec)
    | VarDec !Text !(Maybe Text) !Expr !Escaping !Span
    | FunDecs !(NonEmpty FunDec)

data TypeDec = TypeDec
    { typeName :: !Text
    , typeBody :: !TypeDecBody
    , typeSpan :: !Span
    }

data TypeDecBody
    = TypeAlias !Text !Span
    | RecordType ![RecordField] !Span
    | ArrayType !Text !Span

data FunDec = FunDec
    { funName   :: !Text
    , funArgs   :: ![DecField]
    , funResult :: !(Maybe Text)
    , funBody   :: !Expr
    , funSpan   :: !Span
    }

data DecField = DecField
    { decFieldName   :: !Text
    , decFieldEscape :: !Escaping
    , decFieldType   :: !Text
    , decFieldSpan   :: !Span
    }

data RecordField = RecordField
    { recFieldName :: !Text
    , recFieldType :: !Text
    , recFieldSpan :: !Span
    }

data Expr
    = LVal !LVal
    | Nil !Span
    | IntLit !Int !Span
    | StrLit !Text !Span
    | Neg !Expr !Span
    | Binop !Expr !Binop !Expr !Span
    | Record !Text ![Field] !Span
    | Array !Text !Expr !Expr !Span
    | Assign !LVal !Expr !Span
    | If !Expr !Expr !(Maybe Expr) !Span
    | While !Expr !Expr !Span
    | For !Text !Escaping !Expr !Expr !Expr !Span
    | Seq ![Expr] !Span
    | Call !Text ![Expr] !Span
    | Break !Span
    | Let !(NonEmpty Dec) !Expr !Span

exprSpan :: Expr -> Span
exprSpan = \case
    LVal lv         -> lValSpan lv
    Nil s           -> s
    IntLit _ s      -> s
    StrLit _ s      -> s
    Neg _ s         -> s
    Binop _ _ _ s   -> s
    Record _ _ s    -> s
    Array _ _ _ s   -> s
    Assign _ _ s    -> s
    If _ _ _ s      -> s
    While _ _ s     -> s
    For _ _ _ _ _ s -> s
    Seq _ s         -> s
    Call _ _ s      -> s
    Break s         -> s
    Let _ _ s       -> s

setExprSpan :: Span -> Expr -> Expr
setExprSpan s = \case
    LVal lv          -> LVal (setLValSpan s lv)
    Nil _            -> Nil s
    IntLit v _       -> IntLit v s
    StrLit v _       -> StrLit v s
    Neg e _          -> Neg e s
    Binop e1 op e2 _ -> Binop e1 op e2 s
    Record t f _     -> Record t f s
    Array t e1 e2 _  -> Array t e1 e2 s
    Assign l e _     -> Assign l e s
    If e1 e2 e3 _    -> If e1 e2 e3 s
    While e1 e2 _    -> While e1 e2 s
    For v e f t b _  -> For v e f t b s
    Seq xs _         -> Seq xs s
    Call f as _      -> Call f as s
    Break _          -> Break s
    Let ds e _       -> Let ds e s

lValSpan :: LVal -> Span
lValSpan = \case
    Var _ s     -> s
    Dot _ _ s   -> s
    Index _ _ s -> s

setLValSpan :: Span -> LVal -> LVal
setLValSpan s = \case
    Var v _     -> Var v s
    Dot l f _   -> Dot l f s
    Index l i _ -> Index l i s

instance TextBuildable Expr where
    toTextBuilder e = exprBuilder' e "" ""
      where exprBuilder' :: Expr -> Builder -> Builder -> Builder
            exprBuilder' = \case
                LVal lv      -> lValBuilder lv
                Nil _        -> showLeaf "nil"
                IntLit int _ -> showLeaf $ Builder.decimal int
                StrLit str _ -> showLeaf $ stringBuilder str
                Neg e1 _     -> showNode "-" [exprBuilder' e1]
                Binop e1 op e2 _ -> showNode (binopBuilder op) [ exprBuilder' e1
                                                               , exprBuilder' e2
                                                               ]
                Record r fs _ -> showNodeNames (Builder.fromText r <> "{}") $
                    map fieldBuilder fs
                Array a e1 e2 _ -> showNode (Builder.fromText a <> "[]") [ exprBuilder' e1
                                                                         , exprBuilder' e2
                                                                         ]
                Assign lv rv _ -> showNode ":=" [lValBuilder lv, exprBuilder' rv]
                If cond th mel _ ->
                    let initNodes = maybe [] (\el -> [(Just "else", exprBuilder' el)]) mel
                        nodes = (Nothing, exprBuilder' cond)
                              : (Just "then", exprBuilder' th)
                              : initNodes
                    in showNodeNames "if" nodes
                While e1 e2 _ -> showNodeNames "while" [ (Just "do", exprBuilder' e1)
                                                       , (Just "body", exprBuilder' e2)
                                                       ]
                For var esc e1 e2 e3 _ -> showNodeNames "for"
                    [ (Just "var", showLeaf $ Builder.fromText var <> escapingBuilder esc)
                    , (Just "from", exprBuilder' e1)
                    , (Just "to", exprBuilder' e2)
                    , (Just "do", exprBuilder' e3)
                    ]
                Seq es _ -> showNode ";" $ map exprBuilder' es
                Call fn as _ -> showNode (Builder.fromText fn <> "()") $ map exprBuilder' as
                Break _ -> showLeaf "break"
                Let ds res _ ->
                    let initNodes = [(Just "in", exprBuilder' res)]
                        nodes = foldr (\x acc -> (Nothing, decBuilder x) : acc) initNodes ds
                    in showNodeNames "let" nodes

            lValBuilder :: LVal -> Builder -> Builder -> Builder
            lValBuilder lv = case lv of
                Var v _        -> showLeaf (Builder.fromText v)
                Dot x y _      -> showNode "." [lValBuilder x, showLeaf (Builder.fromText y)]
                Index x expr _ -> showNode "[]" [lValBuilder x, exprBuilder' expr]

            decBuilder :: Dec -> Builder -> Builder -> Builder
            decBuilder = \case
                TypeDecs decs -> showNode "types" $ NonEmpty.toList $ fmap typeDecBuilder decs
                VarDec var mtyp val esc _ ->
                    let varBuilder =  Builder.fromText var
                                   <> maybe "" (\typ -> " : " <> Builder.fromText typ) mtyp
                                   <> escapingBuilder esc
                    in showNode ":=" [showLeaf varBuilder, exprBuilder' val]
                FunDecs decs -> showNode "funcs" $ NonEmpty.toList $ fmap funDecBuilder decs

            typeDecBuilder :: TypeDec -> Builder -> Builder -> Builder
            typeDecBuilder TypeDec{..} = showNode (Builder.fromText typeName)
                [typeBodyBuilder typeBody]

            typeBodyBuilder :: TypeDecBody -> Builder -> Builder -> Builder
            typeBodyBuilder = \case
                TypeAlias typ _ -> showLeaf (Builder.fromText typ)
                RecordType fs _ -> showLeaf $ "{" <> recFieldsBuilder fs <> "}"
                ArrayType typ _ -> showLeaf $ Builder.fromText typ <> "[]"

            funDecBuilder :: FunDec -> Builder -> Builder -> Builder
            funDecBuilder FunDec{..} =
                let funText =  Builder.fromText funName
                            <> "(" <> decFieldsBuilder funArgs <> ")"
                            <> maybe "" (\res -> ": " <> Builder.fromText res) funResult
                in showNode funText [exprBuilder' funBody]

            decFieldsBuilder :: [DecField] -> Builder
            decFieldsBuilder = intercalate ", " . map decFieldBuilder
              where decFieldBuilder DecField{..} = Builder.fromText decFieldName <> " : "
                                                <> Builder.fromText decFieldType
                                                <> escapingBuilder decFieldEscape

            recFieldsBuilder :: [RecordField] -> Builder
            recFieldsBuilder = intercalate ", " . map recFieldBuilder
              where recFieldBuilder RecordField{..} =  Builder.fromText recFieldName
                                                    <> " : "
                                                    <> Builder.fromText recFieldType

            binopBuilder :: Binop -> Builder
            binopBuilder = \case
                Add -> "+"
                Sub -> "-"
                Mul -> "*"
                Div -> "/"
                Lt  -> "<"
                Le  -> "<="
                Gt  -> ">"
                Ge  -> ">="
                Ne  -> "<>"
                Eq  -> "="
                And -> "&"
                Or  -> "|"

            fieldBuilder :: Field -> (Maybe Builder, Builder -> Builder -> Builder)
            fieldBuilder Field{..} = ( Just $ Builder.fromText fieldName
                                     , exprBuilder' fieldValue
                                     )

            escapingBuilder :: Escaping -> Builder
            escapingBuilder = \case
                Escaping  -> " [escaping]"
                Remaining -> ""

data LVal
    = Var !Text !Span
    | Dot !LVal !Text !Span
    | Index !LVal !Expr !Span

data Escaping
    = Escaping
    | Remaining

data Binop
    = Add | Sub | Mul | Div
    | Lt | Le | Gt | Ge
    | Ne | Eq | And | Or

data Field = Field
    { fieldName  :: !Text
    , fieldValue :: !Expr
    , fieldSpan  :: !Span
    }

data Span = Span
    { spanFrom :: !Position
    , spanTo   :: !Position
    }

data Position = Position
    { posLine   :: !Natural
    , posColumn :: !Natural
    }
