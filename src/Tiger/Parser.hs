{-# LANGUAGE OverloadedStrings #-}

module Tiger.Parser (parseFromText) where

import Control.Monad (void, when, join)
import Control.Monad.Combinators.Expr
import Data.Char (isAlphaNum, isDigit, isSpace, chr, ord)
import Data.Fix
import Data.Functor (($>))
import Data.Functor.Compose
import Data.Text hiding (empty, foldl1, cons)
import Data.Void
import Text.Megaparsec hiding (pos1)
import Text.Megaparsec.Char hiding (space, printChar)

import Tiger.Expr hiding (SourcePos(..), ($>))

import qualified Data.HashSet as HashSet
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Text.Megaparsec.Char.Lexer as Lexer

import qualified Tiger.Expr as Expr

type Parser = Parsec Void Text

parseFromText :: String -> Text -> Either String (PosExpr UntypedDec)
parseFromText fpath src = case parse pRootExpr fpath src of
    Left err  -> Left $ errorBundlePretty err
    Right res -> Right res

pRootExpr :: Parser (PosExpr UntypedDec)
pRootExpr = space *> pExpr <* eof

pExpr :: Parser (PosExpr UntypedDec)
pExpr = makeExprParser term operators <?> "expression"
  where term =  try pIdFactor
            <|> pIntLit
            <|> pStrLit
            <|> pNil
            <|> pBreak
            <|> pIf
            <|> pWhile
            <|> pFor
            <|> pLet
            <|> parens pSeq

pIdFactor :: Parser (PosExpr UntypedDec)
pIdFactor = do
    next <- join <$>
        lookAhead (pId *> optional
            ((brackets anyButBracket *> optional anyLexeme) <|> Just <$> anyLexeme))
    case next of
        Just '{' -> pRecord -- id {fields}
        Just 'o' -> pArray  -- id [expr] of expr
        Just '(' -> pCall   -- id (args)
        _        -> pLVal   -- id | id [expr] | (id | id [expr]) . lval
  where anyButBracket = some $ satisfy (/=']')
        anyLexeme = lexeme $ satisfy $ const True

operators :: [[Operator Parser (PosExpr UntypedDec)]]
operators = [ [negop]
            , [binop "*" Mul, binop "/" Div]
            , [binop "+" Add, binop "-" Sub]
            , [ binop ">" Gt, binop ">=" Ge
              , binop "<" Lt, binop "<=" Le
              ]
            , [binop "=" Eq, binop "<>" Ne]
            , [binop "&" And]
            , [binop "|" Or]
            ]
  where negop = Prefix (consNeg <$> annotate (operator "-"))
          where consNeg (Ann pos1 _) e@(AnnE pos2 _) = AnnE (pos1 <> pos2) (Neg e)
                consNeg _ _ = error "unexpected"
        binop op cons = InfixL (operator op $> consBinop)
          where consBinop e1@(AnnE pos1 _) e2@(AnnE pos2 _) =
                    AnnE (pos1 <> pos2) (Binop e1 cons e2)
                consBinop _ _ = error "unexpected"

operator :: Text -> Parser ()
operator ">" = void $ lexeme $ try (string ">" <* notFollowedBy (char '='))
operator "<" = void $ lexeme $ try (string "<" <* notFollowedBy (char '=' <|> char '>'))
operator n = void $ symbol n

pIntLit :: Parser (PosExpr UntypedDec)
pIntLit = annotate1 (IntLit <$> lexeme Lexer.decimal <?> "integer")

pStrLit :: Parser (PosExpr UntypedDec)
pStrLit = annotate1
  (   StrLit . pack
  <$> (char '"' *> many strChar <* symbol "\"")
  <?> "string"
  )

strChar :: Parser Char
strChar = printChar <|> escapeChar

printChar :: Parser Char
printChar = satisfy $ \x ->
    x == '!'               ||
    x == ' '               ||
    x >  '"'  && x <  '\\' ||
    x >  '\\' && x <= '~'

escapeChar :: Parser Char
escapeChar = do
    _ <- char '\\'
    c <- lookAhead $ satisfy $ \x ->
        isDigit x ||
        isSpace x ||
        x == 'n'  ||
        x == 't'  ||
        x == '"'  ||
        x == '\\'
    case c of
        'n' -> retChar '\n'
        't' -> retChar '\t'
        '"' -> retChar '"'
        '\\' -> retChar '\\'
        x | isDigit x -> pAsciiCode
          | otherwise -> space1 *> char '\\' *> strChar
  where retChar c = satisfy (const True) $> c
        pAsciiCode = do
            offset <- getOffset
            code <- read3 <$> digitChar <*> digitChar <*> digitChar
            when (code > 127) $
                throwError offset "non-ascii character's code is given"
            return $ chr code
          where read3 x y z = (num x * 100) + (num y * 10) + num z
                num x = ord x - ord '0'

pNil :: Parser (PosExpr UntypedDec)
pNil = annotate1 (Nil <$ reserved "nil" <?> "nil")

pBreak :: Parser (PosExpr UntypedDec)
pBreak = annotate1 (Break <$ reserved "break" <?> "break")

pLVal :: Parser (PosExpr UntypedDec)
pLVal = annotate1 $ do
    lv <- lval
    mrv <- optional (symbol ":=" *> pExpr)
    return $ maybe (LVal lv) (Assign lv) mrv
  where lval = unExpr . foldl1 consDot <$> sepBy1 term (symbol ".")
        term = do
            beg <- getSourcePos
            var <- annotate1 (LVal . Var <$> pId)
            mind <- optional (brackets (unannotate <$> pExpr))
            end <- getSourcePos
            return $ maybe var (AnnE (fromSourcePos beg end) . LVal . Index var) mind
        -- a . Var b           -> Dot a b
        -- a . Index (Var b) c -> Index (Dot a b) c
        consDot a@(AnnE pos1 _) (AnnE pos2 lv) = case lv of
            LVal (Var b) -> AnnE (pos1 <> pos2) (LVal $ Dot a b 0)
            LVal (Index (AnnE pos3 (LVal (Var b))) c) ->
                AnnE (pos1 <> pos2)
                    (LVal $ Index (AnnE (pos1 <> pos3) (LVal $ Dot a b 0)) c)
            _ -> error "unexpected"
        consDot _ _ = error "unexpected"
        unExpr (AnnE _ (LVal lv)) = lv
        unExpr _ = error "unexpected"

pRecord :: Parser (PosExpr UntypedDec)
pRecord = annotate1
  (   Record
  <$> pId
  <*> braces (sepBy (RecordField <$> pId <*> (symbol "=" *> pExpr)) (symbol ","))
  <?> "record creation"
  )

pArray :: Parser (PosExpr UntypedDec)
pArray = annotate1
  (   Array
  <$> pId
  <*> brackets pExpr
  <*> (reserved "of" *> pExpr)
  <?> "array creation"
  )

pCall :: Parser (PosExpr UntypedDec)
pCall = annotate1
  (   Call
  <$> pId
  <*> parens (sepBy pExpr (symbol ","))
  <?> "function call"
  )

pIf :: Parser (PosExpr UntypedDec)
pIf = annotate1
  (   If
  <$> (reserved "if" *> pExpr)
  <*> (reserved "then" *> pExpr)
  <*> optional (reserved "else" *> pExpr)
  <?> "if"
  )

pWhile :: Parser (PosExpr UntypedDec)
pWhile = annotate1
  (   While
  <$> (reserved "while" *> pExpr)
  <*> (reserved "do" *> pExpr)
  <?> "while"
  )

pFor :: Parser (PosExpr UntypedDec)
pFor = annotate1
  (   For
  <$> (reserved "for" *> pId)
  <*> (return Remaining)
  <*> (symbol ":=" *> pExpr)
  <*> (reserved "to" *> pExpr)
  <*> (reserved "do" *> pExpr)
  <?> "for"
  )

pSeq :: Parser (PosExpr UntypedDec)
pSeq = do
    beg <- getSourcePos
    x <- pExpr
    mxs <- optional (symbol ";" *> sepBy1 pExpr (symbol ";"))
    end <- getSourcePos
    return $ case mxs of
        Nothing -> x
        Just xs -> AnnE (fromSourcePos beg end) (Seq $ x:xs)

pLet :: Parser (PosExpr UntypedDec)
pLet = annotate1
  (   Let
  <$> (reserved "let" *> some pDec)
  <*> (reserved "in" *> pSeq)
  <*  (reserved "end")
  <?> "let"
  )

type Dec = UntypedDec (PosExpr UntypedDec)
type VarDec = UntypedVar (PosExpr UntypedDec)
type FunDec = UntypedFun (PosExpr UntypedDec)

pDec :: Parser Dec
pDec =  UntypedTypeDec <$> pTypeDec
    <|> UntypedVarDec  <$> pVarDec
    <|> UntypedFunDec  <$> pFunDec

pTypeDec :: Parser UntypedType
pTypeDec =  UntypedType
        <$> (reserved "type" *> pId)
        <*> (symbol "=" *> pTypeRVal)
        <?> "type declaration"

pTypeRVal :: Parser TypeRVal
pTypeRVal =  (TypeId <$> try pId)
         <|> (TypeRecord <$> braces (sepBy1 pRecordField (symbol ",")))
         <|> (TypeArray <$> (reserved "array" *> reserved "of" *> pId))

pVarDec :: Parser VarDec
pVarDec =  UntypedVar
       <$> (reserved "var" *> pId)
       <*> optional (try $ symbol ":" *> notFollowedBy (char '=') *> pId)
       <*> (symbol ":=" *> pExpr)
       <?> "variable declaration"

pFunDec :: Parser FunDec
pFunDec =  UntypedFun
       <$> (reserved "function" *> pId)
       <*> parens (sepBy pFunArg (symbol ","))
       <*> optional (symbol ":" *> pId)
       <*> (symbol "=" *> pExpr)
       <?> "function declaration"

pRecordField :: Parser (RecordField Text)
pRecordField = RecordField <$> pId <*> (symbol ":" *> pId)

pFunArg :: Parser UntypedFunArg
pFunArg = UntypedFunArg <$> pId <*> (symbol ":" *> pId)

reserved :: Text -> Parser ()
reserved = void . symbol

pId :: Parser Text
pId = lexeme $ do
    offset <- getOffset
    ident <-
        Text.cons
        <$> letterChar
        <*> takeWhileP Nothing (\x -> isAlphaNum x || x == '_')
    when (ident `HashSet.member` reservedWords) $
        throwError offset $ "unexpected reserved word " ++ unpack ident
    return ident

space :: Parser ()
space = Lexer.space space1 empty (Lexer.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme space

symbol :: Text -> Parser Text
symbol = Lexer.symbol space

braces, brackets, parens :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")
brackets = between (symbol "[") (symbol "]")
parens = between (symbol "(") (symbol ")")

reservedWords :: HashSet.HashSet Text
reservedWords =
    HashSet.fromList [ "type", "array", "of", "var", "function", "let", "in", "end"
                     , "if", "then", "else", "nil", "while", "for", "do", "to", "break"
                     ]

annotate :: Parser a -> Parser (Ann SrcSpan a)
annotate p = do
    beg <- getSourcePos
    res <- p
    end <- getSourcePos
    return $ Ann (fromSourcePos beg end) res

annotate1 :: Functor f
          => Parser (f (Fix (AnnF SrcSpan f)))
          -> Parser (Fix (AnnF SrcSpan f))
annotate1 = fmap annToAnnF . annotate

unannotate :: Fix (Compose (Ann ann) g) -> g (Fix (Compose (Ann ann) g))
unannotate = annotated . getCompose . unFix

fromSourcePos :: SourcePos -> SourcePos -> SrcSpan
fromSourcePos bpos epos = SrcSpan (convert bpos) (convert epos)
  where convert (SourcePos fpath ln cl) = Expr.SourcePos fpath (unPos ln) (unPos cl)

throwError :: Int -> String -> Parser a
throwError offset = parseError . FancyError offset . Set.singleton . ErrorFail
