module Parser (
    parseFromFile,
    exprRoot
    ) where

import Lex
import Text.Parsec.Prim hiding (token)
import Text.Parsec.Combinator
import Text.Parsec.Pos(setSourceLine, setSourceColumn, sourceName, sourceLine, sourceColumn)
import Text.Parsec.Expr
import Data.Functor.Identity
import Absyn hiding (SourcePos(..))
import qualified Absyn as A

type Parser a = Parsec [PosToken] () a

parseFromFile :: Parser a -> FilePath -> IO (Either String a)
parseFromFile p f = readFile f >>= \src -> case alexScanTokens src of
    Left err -> return $ Left err
    Right toks -> case parse p f toks of
        Left err -> return $ Left $ show err
        Right res -> return $ return res

satisfy :: (Token -> Bool) -> Parser Token
satisfy pred = tokenPrim show nextPos (\(PosToken _ t) -> if pred t then Just t else Nothing)
  where nextPos pos _ ts = case ts of
          (PosToken (AlexPn _ ln cl) _:_) -> setSourceColumn (setSourceLine pos ln) cl
          _ -> pos

token :: Token -> Parser Token
token = satisfy . (==)

idTok :: Parser Token
idTok = satisfy $ \tok -> case tok of
    IdTok _ -> True
    _ -> False

intTok :: Parser Token
intTok = satisfy $ \tok -> case tok of
    IntTok _ -> True
    _ -> False

strTok :: Parser Token
strTok = satisfy $ \tok -> case tok of
    StrTok _ -> True
    _ -> False

srcPos :: Parser A.SourcePos
srcPos = getPosition >>= \sc -> return $ A.SourcePos (sourceName sc) (sourceLine sc) (sourceColumn sc)

brackets :: Parser a -> Parser a
brackets p = token LBracketTok *> p <* token RBracketTok

parens :: Parser a -> Parser a
parens p = token LParTok *> p <* token RParTok

braces :: Parser a -> Parser a
braces p = token LBraceTok *> p <* token RBraceTok

exprRoot :: Parser Expr
exprRoot = expr <* token EOFTok

dec :: Parser Dec
dec = typeParser <|> varParser <|> funParser
  where typeParser = srcPos >>= \sc -> token TypeTok >> (idTok <* token EqTok) >>= \(IdTok tid) -> ty >>= \rv -> return $ TypeDec sc $ UntypedType tid rv
        ty = (idTok >>= \(IdTok n) -> return $ TypeId n)                                     <|>
             (braces (tyfields sepBy1) >>= return . TypeRecord)                                       <|>
             (token ArrayTok >> token OfTok >> idTok >>= \(IdTok n) -> return $ TypeArray n)
        tyfields sep = sep (idTok >>= \(IdTok v) -> (token ColonTok >> idTok) >>= \(IdTok t) -> return (v,t)) (token ComaTok)
        varParser = srcPos >>= \sc -> token VarTok >> idTok >>= \(IdTok v) ->
            optionMaybe (token ColonTok >> idTok >>= \(IdTok n) -> return n) >>=
                \mtid -> token AssignTok >> expr >>= return . VarDec sc . UntypedVar v mtid
        funParser = srcPos >>= \sc -> token FunTok >> idTok >>= \(IdTok f) ->
            parens (tyfields sepBy) >>= \prs ->
                optionMaybe (token ColonTok >> idTok >>= \(IdTok n) -> return n) >>=
                    \mtid -> token EqTok >> expr >>= return . FunDec sc . UntypedFun f prs mtid

expr :: Parser Expr
expr = buildExpressionParser operators term
  where term = (try idFactor)                                                                                  <|>
               lvalFactor                                                                                      <|>
               (parens seqParser)                                                                              <|>
               (Nil <$> srcPos <* token NilTok)                                                                <|>
               (srcPos >>= \sc -> intTok >>= \(IntTok v) -> return $ IntLit sc v)                              <|>
               (srcPos >>= \sc -> strTok >>= \(StrTok v) -> return $ StrLit sc v)                              <|>
               ifParser                                                                                        <|>
               (While <$> srcPos <*> (token WhileTok *> expr) <*> (token DoTok *> expr))                       <|>
               forParser                                                                                       <|>
               (Break <$> srcPos <* token BreakTok)                                                            <|>
               (Let <$> srcPos <*> (token LetTok *> many1 dec) <*> (token InTok *> seqParser <* token EndTok))
          where ifParser = (If <$> srcPos <*> (token IfTok *> expr) <*> (token ThenTok *> expr) <*> (optionMaybe $ token ElseTok *> expr))
                forParser = srcPos >>= \sc -> token ForTok >>
                    (idTok <* token AssignTok) >>= \(IdTok v) ->
                        (expr <* token ToTok) >>= \e1 ->
                            (expr <* token DoTok) >>= \e2 ->
                                expr >>= \e3 -> return $ For sc v False e1 e2 e3
                lvalFactor = srcPos >>= \sc -> lval >>= \lv ->
                    optionMaybe (token AssignTok >> expr) >>=
                        return . maybe (LVal sc lv) (Assign sc lv)
                seqParser = srcPos >>= \sc -> sepBy1 expr (token SemicolonTok) >>=
                    \exprs -> return $ if length exprs == 1
                                          then head exprs
                                          else Seq sc exprs
                idFactor = srcPos >>= \sc -> idTok >>= \(IdTok n) ->
                    (parens (sepBy expr (token ComaTok)) >>= return . Call sc n)                       <|>
                    (braces (sepBy field (token ComaTok)) >>= return . Record sc n)                     <|>
                    (brackets expr >>= \e1 -> token OfTok >> expr >>= \e2 -> return $ Array sc n e1 e2)
                field = idTok >>= \(IdTok n) -> token EqTok >> expr >>= \exp -> return (n, exp)

lval :: Parser LVal
lval = ident >>= \lv -> optionMaybe (many1 (token DotTok >> ident)) >>= return . maybe lv (\rest -> dotSeq $ lv:rest)
  where ident = idTok >>= \(IdTok name) -> optionMaybe (brackets expr) >>= return . maybe (LId name) (LArr (LId name))
        dotSeq (x:y:xs)
          | null xs = z
          | otherwise = dot z (dotSeq xs)
          where z = dot x y
        dotSeq [x] = x
        dot x y = case y of
            LId name -> LDot x name undefined
            LDot name sub off -> LDot (deep x name) sub off
            LArr name ind -> LArr (deep x name) ind
        deep x lv = case lv of
            LId name -> LDot x name undefined
            LArr name ids -> LArr (deep x name) ids
            LDot y z off -> LDot (deep x y) z off

operators :: OperatorTable [PosToken] () Identity Expr
operators = [ [ Prefix (token MinusTok >> Neg <$> srcPos) ] 
            , [ binop StarTok Mul AssocLeft
              , binop SlashTok Div AssocLeft
              ]
            , [ binop PlusTok Add AssocLeft
              , binop MinusTok Sub AssocLeft
              ]
            , [ binop EqTok Eq AssocNone
              , binop NeTok Ne AssocNone
              , binop GtTok Gt AssocNone
              , binop LtTok Lt AssocNone
              , binop GeTok Ge AssocNone
              , binop LeTok Le AssocNone
              ]
            , [ binop AmpTok And AssocNone ]
            , [ binop VLineTok Or AssocNone ]
            ]
  where binop tok cons assoc = Infix (token tok >> srcPos >>= \sc -> return (\e1 e2 -> BinOp sc e1 cons e2)) assoc
