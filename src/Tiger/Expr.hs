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

    , exprToText
    , exprSpan
    , setExprSpan
    , lValSpan
    , setLValSpan
    ) where

import           Data.List.NonEmpty (NonEmpty)
import           Data.Text          (Text)
import           Numeric.Natural    (Natural)
import           Prelude            hiding (span)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text          as Text

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

exprToText :: Expr -> Text
exprToText e = exprToText' e "" ""
  where exprToText' :: Expr -> Text -> Text -> Text
        exprToText' expr = case expr of
            LVal lv      -> lValToText lv
            Nil _        -> showLeaf "nil"
            IntLit int _ -> showLeaf (Text.pack $ show int)
            StrLit str _ -> showLeaf (Text.pack $ show str)
            Neg e1 _     -> showNode "-" [exprToText' e1]
            Binop e1 op e2 _ -> showNode (binopToText op) [ exprToText' e1
                                                          , exprToText' e2
                                                          ]
            Record r fs _ -> showNodeNames (r <> "{}") (map fieldToText fs)
            Array a e1 e2 _ -> showNode (a <> "[]") [exprToText' e1, exprToText' e2]
            Assign lv rv _ -> showNode ":=" [lValToText lv, exprToText' rv]
            If cond th mel _ ->
                let initNodes = maybe [] (\el -> [("else", exprToText' el)]) mel
                    nodes = ("", exprToText' cond)
                          : ("then", exprToText' th)
                          : initNodes
                in showNodeNames "if" nodes
            While e1 e2 _ -> showNodeNames "while" [ ("do", exprToText' e1)
                                                   , ("body", exprToText' e2)
                                                   ]
            For var esc e1 e2 e3 _ -> showNodeNames "for" $
                [ ("var", showLeaf $ var <> escapingToText esc)
                , ("from", exprToText' e1)
                , ("to", exprToText' e2)
                , ("do", exprToText' e3)
                ]
            Seq es _ -> showNode ";" $ map exprToText' es
            Call fn as _ -> showNode (fn <> "()") $ map exprToText' as
            Break _ -> showLeaf "break"
            Let ds res _ ->
                let initNodes = [("in", exprToText' res)]
                    nodes = foldr (\x acc -> ("", decToText x) : acc) initNodes ds
                in showNodeNames "let" nodes

        lValToText :: LVal -> Text -> Text -> Text
        lValToText lv = case lv of
            Var v _        -> showLeaf v
            Dot x y _      -> showNode "." [lValToText x, showLeaf y]
            Index x expr _ -> showNode "[]" [lValToText x, exprToText' expr]

        decToText :: Dec -> Text -> Text -> Text
        decToText = \case
            TypeDecs decs -> showNode "types" $ NonEmpty.toList $ fmap typeDecToText decs
            VarDec var mtyp val esc _ ->
                let varText =  var
                            <> maybe "" (\typ -> " : " <> typ) mtyp
                            <> escapingToText esc
                in showNode ":=" [showLeaf varText, exprToText' val]
            FunDecs decs -> showNode "funcs" $ NonEmpty.toList $ fmap funDecToText decs

        typeDecToText :: TypeDec -> Text -> Text -> Text
        typeDecToText TypeDec{..} = showNode typeName [typeBodyToText typeBody]

        typeBodyToText :: TypeDecBody -> Text -> Text -> Text
        typeBodyToText = \case
            TypeAlias typ _ -> showLeaf typ
            RecordType fs _ -> showLeaf $ "{" <> showRecFields fs <> "}"
            ArrayType typ _ -> showLeaf $ typ <> "[]"

        funDecToText :: FunDec -> Text -> Text -> Text
        funDecToText FunDec{..} =
            let funText =  funName <> "(" <> showDecFields funArgs <> ")"
                        <> maybe "" (\res -> ": " <> res) funResult
            in showNode funText [exprToText' funBody]

        showDecFields :: [DecField] -> Text
        showDecFields = Text.intercalate ", " . map showDecField
            where showDecField DecField{..} =  decFieldName <> " : " <> decFieldType
                                            <> escapingToText decFieldEscape

        showRecFields :: [RecordField] -> Text
        showRecFields = Text.intercalate ", " . map showRecField
          where showRecField RecordField{..} =  recFieldName <> " : " <> recFieldType

        binopToText :: Binop -> Text
        binopToText = \case
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

        fieldToText :: Field -> (Text, Text -> Text -> Text)
        fieldToText Field {..} = (fieldName, exprToText' fieldValue)

        escapingToText :: Escaping -> Text
        escapingToText = \case
            Escaping  -> " [escaping]"
            Remaining -> ""

showNodeNames :: Text -> [(Text, Text -> Text -> Text)] -> Text -> Text -> Text
showNodeNames node xs pr cpr = pr <> node <> Text.cons '\n' (showXs pr cpr)
  where showXs :: Text -> Text -> Text
        showXs = snd (foldr (\a b -> (False, go a b)) (True, \_ _ -> "") xs)

        go :: (Text, Text -> Text -> Text)
           -> (Bool, Text -> Text -> Text)
           -> Text
           -> Text
           -> Text
        go (name, pfunc) (isLast, fs) goPr goCpr
          | Text.null name = pfunc newPr newCpr <> rest
          | otherwise = showName <> rest
          where rest = fs goPr goCpr
                (newPr, newCpr) = if isLast
                                     then (goCpr <> "└── ", goCpr <> "    ")
                                     else (goCpr <> "├── ", goCpr <> "│   ")
                showName =  newPr <> name
                         <> Text.cons '\n' (pfunc (newCpr <> "└── ") (newCpr <> "    "))


showNode :: Text -> [Text -> Text -> Text] -> Text -> Text -> Text
showNode node = showNodeNames node . map (\x -> ("",x))

showLeaf :: Text -> Text -> Text -> Text
showLeaf s pr _ = pr <> s <> "\n"

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
