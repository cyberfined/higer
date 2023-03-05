module Tiger.Parser (parse) where

import           Control.Monad                  (replicateM, void, when, join)
import           Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import           Data.Char                      (chr, digitToInt, isAlpha, isAlphaNum,
                                                 isDigit, isPrint, toUpper)
import           Data.Functor                   (($>))
import           Data.HashSet                   (HashSet)
import           Data.List.NonEmpty             (NonEmpty (..))
import           Data.Text                      (Text)
import           Data.Void                      (Void)
import           Prelude                        hiding (break, init, sequence, span)
import           Text.Megaparsec                hiding (getSourcePos, parse, some)
import           Text.Megaparsec.Char           (char, char', digitChar, space1,
                                                 spaceChar)
import qualified Text.Megaparsec.Char.Lexer     as Lexer

import           Tiger.Expr

import qualified Data.HashSet                   as HashSet
import qualified Data.Set                       as Set
import qualified Data.Text                      as Text
import qualified Text.Megaparsec                as Megaparsec

type Parser = Parsec Void Text

parse :: FilePath -> Text -> Either Text Expr
parse file src = case Megaparsec.parse rootExpr file src of
    Left err  -> Left $ Text.pack $ errorBundlePretty err
    Right res -> Right res

rootExpr :: Parser Expr
rootExpr = space *> expr <* eof

expr :: Parser Expr
expr =  makeExprParser term operators <?> "expression"
  where term =  try idFactor
            <|> nil
            <|> integer
            <|> string
            <|> if'
            <|> while
            <|> for
            <|> break
            <|> let'
            <|> sequence parens sepBy

        operators :: [[Operator Parser Expr]]
        operators = [ [minus]
                    , [binop InfixL "*" Mul, binop InfixL "/" Div]
                    , [binop InfixL "+" Add, binop InfixL "-" Sub]
                    , [ binop InfixN "<" Lt, binop InfixN "<=" Le
                      , binop InfixN ">" Gt, binop InfixN ">=" Ge
                      ]
                    , [binop InfixN "=" Eq, binop InfixN "<>" Ne]
                    , [binop InfixL "&" And]
                    , [binop InfixL "|" Or]
                    ]

        operator :: Text -> Parser ()
        operator ">" = void $ lexeme $ try (char '>' <* notFollowedBy (char '='))
        operator "<" = void $ lexeme $
            try (char '<' <* notFollowedBy (char '=' <|> char '>'))
        operator op = void $ symbol op

        minus :: Operator Parser Expr
        minus = Prefix $ do
            from <- getSourcePos
            operator "-"
            pure $ \e ->
                let to = spanTo $ exprSpan e
                    span = Span from to
                in Neg e span

        binop :: (Parser (Expr -> Expr -> Expr) -> Operator Parser Expr)
              -> Text
              -> Binop
              -> Operator Parser Expr
        binop assoc op cons = assoc $ operator op $> \e1 e2 ->
            let span = Span (spanFrom $ exprSpan e1) (spanTo $ exprSpan e2)
            in Binop e1 cons e2 span

idFactor :: Parser Expr
idFactor = do
    next <- join <$>
        lookAhead (identifier *> optional
            ((brackets anyButBracket *> optional anyLexeme) <|> Just <$> anyLexeme))
    case next of
        Just '{' -> record
        Just 'o' -> array
        Just '(' -> call
        _        -> lValue
  where anyButBracket :: Parser ()
        anyButBracket = void $ many $ satisfy (/= ']')

        anyLexeme :: Parser Char
        anyLexeme = lexeme $ anySingle

nil :: Parser Expr
nil = annotate (reserved NilWord $> Nil) <?> "nil"

integer :: Parser Expr
integer = lexeme (annotate (IntLit <$> Lexer.decimal) <?> "integer")

lValue :: Parser Expr
lValue = do
    lv <- parser <?> "l-value"
    lookAhead (optional $ symbol ":=") >>= \case
        Just{}  -> do
            void $ symbol ":="
            rv <- expr
            to <- getSourcePos
            let from = spanFrom $ lValSpan lv
            let span = Span from to
            pure $ Assign lv rv span
        Nothing -> pure $ LVal lv
  where parser :: Parser LVal
        parser = do
            from <- getSourcePos
            varName <- identifier
            to <- getSourcePos
            let var = Var varName (Span from to)
            lvalue' var from

        lvalue' :: LVal -> Position -> Parser LVal
        lvalue' varOrIndex from = do
            lookAhead (optional anySingle) >>= \case
                Just '[' -> do
                    index <- brackets expr
                    to <- getSourcePos
                    lvalue' (Index varOrIndex index (Span from to)) from
                Just '.' -> do
                    void $ symbol "."
                    field <- identifier
                    to <- getSourcePos
                    lvalue' (Dot varOrIndex field (Span from to)) from
                _        -> pure varOrIndex

string :: Parser Expr
string = annotate (StrLit <$> parser) <?> "string"
  where parser :: Parser Text
        parser = between (char '"') (lexeme $ char '"') (betweenQuotes "")

        betweenQuotes :: Text -> Parser Text
        betweenQuotes str = do
            part <- takeWhileP (Just "printable character") isRegularChar
            lookAhead (optional anySingle) >>= \case
                Just '\\' -> do
                    escChr <- escapeSequence
                    betweenQuotes (str <> Text.snoc part escChr)
                _ -> pure $ str <> part

        isRegularChar :: Char -> Bool
        isRegularChar c = isPrint c && c /= '\\' && c /= '"'

        strChar :: Parser Char
        strChar = (satisfy isRegularChar <?> "printable character") <|> escapeSequence

        escapeSequence :: Parser Char
        escapeSequence = do
            void $ char '\\'
            x <- lookAhead $  char 'n'
                          <|> char 't'
                          <|> char '^'
                          <|> char '"'
                          <|> char '\\'
                          <|> digitChar
                          <|> spaceChar
            case x of
                'n'  -> anySingle $> '\n'
                't'  -> anySingle $> '\t'
                '^'  -> controlChar
                '"'  -> anySingle $> '"'
                '\\' -> anySingle $> '\\'
                _ | isDigit x -> asciiCode
                  | otherwise -> space1 *> char '\\' *> strChar

        controlChar :: Parser Char
        controlChar = do
            void $ char '^'
            x <- char '@' <|>
                 char' 'G' <|>
                 char' 'H' <|>
                 char' 'I' <|>
                 char' 'J' <|>
                 char' 'K' <|>
                 char' 'L' <|>
                 char' 'M' <|>
                 char' 'Z'
            case toUpper x of
                '@' -> pure '\0'
                'G' -> pure '\a'
                'H' -> pure '\b'
                'I' -> pure '\t'
                'J' -> pure '\n'
                'K' -> pure '\v'
                'L' -> pure '\f'
                'M' -> pure '\r'
                _   -> pure '\032'

        asciiCode :: Parser Char
        asciiCode = do
            offset <- getOffset
            [a, b, c] <- map digitToInt <$> replicateM 3 digitChar
            let code = a * 100 + b * 10 + c
            when (code > 127) $
                throwError offset "wrong ascii character code"
            pure $ chr code

sequence :: (Parser [Expr] -> Parser [Expr])
         -> (Parser Expr -> Parser Text -> Parser [Expr])
         -> Parser Expr
sequence withParens sep = do
    from <- getSourcePos
    xs <- withParens $ (sep expr $ symbol ";")
    to <- getSourcePos
    let span = Span from to
    case xs of
        [x] -> pure $ setExprSpan span x
        _   -> pure $ Seq xs span

record :: Parser Expr
record = annotate
  (   Record
  <$> identifier
  <*> braces (sepBy field $ symbol ",")
  <?> "record"
  )
  where field = annotate
          (   Field
          <$> identifier
          <*> (symbol "=" *> expr)
          )

call :: Parser Expr
call = annotate
  (   Call
  <$> identifier
  <*> parens (sepBy expr $ symbol ",")
  <?> "call"
  )

array :: Parser Expr
array = annotate
  (   Array
  <$> identifier
  <*> brackets expr
  <*> (reserved OfWord *> expr)
  <?> "array"
  )

if' :: Parser Expr
if' = annotate
  (   If
  <$> (reserved IfWord *> expr)
  <*> (reserved ThenWord *> expr)
  <*> optional (reserved ElseWord *> expr)
  <?> "if"
  )

while :: Parser Expr
while = annotate
  (   While
  <$> (reserved WhileWord *> expr)
  <*> (reserved DoWord *> expr)
  <?> "while"
  )

for :: Parser Expr
for = annotate
  (   For
  <$> (reserved ForWord *> identifier)
  <*> pure Remaining
  <*> (symbol ":=" *> expr)
  <*> (reserved ToWord *> expr)
  <*> (reserved DoWord *> expr)
  <?> "for"
  )

break :: Parser Expr
break = annotate (reserved BreakWord $> Break)

let' :: Parser Expr
let' = annotate
  (   Let
  <$> (reserved LetWord *> some dec)
  <*> (reserved InWord *> sequence id sepBy1 <* reserved EndWord)
  <?> "let"
  )

dec :: Parser Dec
dec = typeDecs <|> varDec <|> funDecs

typeDecs :: Parser Dec
typeDecs = TypeDecs <$> some typeDec
  where typeDec :: Parser TypeDec
        typeDec = annotate
          (   TypeDec
          <$> (reserved TypeWord *> identifier)
          <*> (symbol "=" *> typeBody)
          <?> "type declaration"
          )
        typeBody :: Parser TypeDecBody
        typeBody = annotate
          (   (reserved ArrayWord *> reserved OfWord *> (ArrayType <$> identifier))
          <|> (RecordType <$> braces decFields)
          <|> (TypeAlias <$> identifier)
          )

varDec :: Parser Dec
varDec = annotate $ do
    reserved VarWord
    name <- identifier
    void $ char ':'
    optional (char '=') >>= \case
        Just{}  -> do
            space
            value <- expr
            pure $ VarDec name Nothing value Remaining
        Nothing -> do
            space
            typ <- identifier
            void $ symbol ":="
            value <- expr
            pure $ VarDec name (Just typ) value Remaining

funDecs :: Parser Dec
funDecs = FunDecs <$> some funDec
  where funDec = annotate
          (   FunDec
          <$> (reserved FunctionWord *> identifier)
          <*> (parens decFields)
          <*> optional (symbol ":" *> identifier)
          <*> (symbol "=" *> expr)
          <?> "function declaration"
          )

decFields :: Parser [DecField]
decFields = sepBy decField $ symbol ","
  where decField = annotate
          (   DecField
          <$> identifier
          <*> pure Remaining
          <*> (symbol ":" *> identifier)
          )

data ReservedWord
    = TypeWord
    | ArrayWord
    | OfWord
    | VarWord
    | FunctionWord
    | LetWord
    | InWord
    | EndWord
    | NilWord
    | IfWord
    | ThenWord
    | ElseWord
    | WhileWord
    | DoWord
    | ForWord
    | ToWord
    | BreakWord
    deriving (Bounded, Enum)

reserved :: ReservedWord -> Parser ()
reserved = void . symbol . reservedWordToText

reservedWordToText :: ReservedWord -> Text
reservedWordToText = \case
    TypeWord     -> "type"
    ArrayWord    -> "array"
    OfWord       -> "of"
    VarWord      -> "var"
    FunctionWord -> "function"
    LetWord      -> "let"
    InWord       -> "in"
    EndWord      -> "end"
    NilWord      -> "nil"
    IfWord       -> "if"
    ThenWord     -> "then"
    ElseWord     -> "else"
    WhileWord    -> "while"
    DoWord       -> "do"
    ForWord      -> "for"
    ToWord       -> "to"
    BreakWord    -> "break"

reservedWords :: HashSet Text
reservedWords = HashSet.fromList $ map (reservedWordToText . toEnum)
                                 $ [fromEnum from..fromEnum to]
  where from :: ReservedWord
        from = minBound

        to :: ReservedWord
        to = maxBound

identifier :: Parser Text
identifier = lexeme $ do
    offset <- getOffset
    x <- satisfy isAlpha
    xs <- takeWhileP (Just "alphanumeric character") (\c -> isAlphaNum c || c == '_')
    let str = Text.cons x xs
    when (HashSet.member str reservedWords) $ do
        throwError offset $ "unexpected reserved word" ++ Text.unpack str
    pure str

some :: Parser a -> Parser (NonEmpty a)
some m = (:|) <$> m <*> many m

space :: Parser ()
space = Lexer.space space1 empty (Lexer.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

symbol :: Text -> Parser Text
symbol = Lexer.symbol space

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

getSourcePos :: Parser Position
getSourcePos = do
    Megaparsec.SourcePos _ linePos columnPos <- Megaparsec.getSourcePos
    let line = fromIntegral $ Megaparsec.unPos linePos
    let column = fromIntegral $ Megaparsec.unPos columnPos
    pure $ Position line column

annotate :: Parser (Span -> a) -> Parser a
annotate m = do
    from <- getSourcePos
    f <- m
    to <- getSourcePos
    let span = Span from to
    pure $ f span

throwError :: Int -> String -> Parser a
throwError offset = parseError . FancyError offset . Set.singleton . ErrorFail
