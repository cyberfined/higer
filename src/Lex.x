{
module Lex(
    PosToken(..),
    Token(..),
    AlexPosn(..),
    alexScanTokens,
    ) where

import Data.Char (chr)
import Numeric (readDec)
}

%wrapper "monadUserState"

$digit      = 0-9
$alpha      = [a-zA-Z]
$whitespace = [\ \t]

rules :-
  <0> $whitespace+                    ;
  <0> \n                              { skip }
  <0> "/*"[.\n]*"*/"                  ;
  <0> "type"                          { retTok TypeTok }
  <0> "array"                         { retTok ArrayTok }
  <0> "of"                            { retTok OfTok }
  <0> "var"                           { retTok VarTok }
  <0> "nil"                           { retTok NilTok }
  <0> "function"                      { retTok FunTok }
  <0> "let"                           { retTok LetTok }
  <0> "in"                            { retTok InTok }
  <0> "end"                           { retTok EndTok }
  <0> "if"                            { retTok IfTok }
  <0> "then"                          { retTok ThenTok }
  <0> "else"                          { retTok ElseTok }
  <0> "while"                         { retTok WhileTok }
  <0> "do"                            { retTok DoTok }
  <0> "for"                           { retTok ForTok }
  <0> "to"                            { retTok ToTok }
  <0> "break"                         { retTok BreakTok }
  <0> ","                             { retTok ComaTok }
  <0> :                               { retTok ColonTok }
  <0> \;                              { retTok SemicolonTok }
  <0> \.                              { retTok DotTok }
  <0> \(                              { retTok LParTok }
  <0> \)                              { retTok RParTok }
  <0> \[                              { retTok LBracketTok }
  <0> \]                              { retTok RBracketTok }
  <0> \{                              { retTok LBraceTok }
  <0> \}                              { retTok RBraceTok }
  <0> \+                              { retTok PlusTok }
  <0> \-                              { retTok MinusTok }
  <0> \*                              { retTok StarTok }
  <0> \/                              { retTok SlashTok }
  <0> &                               { retTok AmpTok }
  <0> \|                              { retTok VLineTok }
  <0> \=                              { retTok EqTok }
  <0> :\=                             { retTok AssignTok }
  <0> \>                              { retTok GtTok }
  <0> \>\=                            { retTok GeTok }
  <0> \<                              { retTok LtTok }
  <0> \<\=                            { retTok LeTok }
  <0> \<\>                            { retTok NeTok }
  <0> $alpha [$alpha $digit \_]*      { \(p, _, _, s) l -> return $ PosToken p (IdTok $ take l s) }
  <0> $digit+                         { \(p, _, _, s) l -> return $ PosToken p (IntTok $ read $ take l s) }
  <0> \"                              { beginString `andBegin` string_state }
  <0> .                               { \_ _ -> lexerError "Illegal character" }
  <string_state> \\n                  { addCharToString '\n' }
  <string_state> \\t                  { addCharToString '\t' }
  <string_state> \\$digit$digit$digit { addDecimalToString }
  <string_state> \\\"                 { addCharToString '"' }
  <string_state> \\\\                 { addCharToString '\\' }
  <string_state> \\[\ \n\t]+\\        ;
  <string_state> \\                   { \_ _ -> lexerError "Illegal escape sequence" }
  <string_state> \"                   { endString `andBegin` init_state }
  <string_state> .                    { addCurCharToString }
  <string_state> \n                   { \_ _ -> lexerError "Illegal newline character in string literal" }
{

alexScanTokens :: String -> Either String [PosToken]
alexScanTokens str = runAlex str loop
  where loop = alexMonadScan >>= \tok -> if isEOF tok
                                            then return [tok]
                                            else loop >>= \rest -> return $ tok:rest
        isEOF tok = case tok of
            PosToken _ EOFTok -> True
            _ -> False

init_state = 0

retTok :: Token -> AlexInput -> Int -> Alex PosToken
retTok tok (pos, _, _, _) _ = return $ PosToken pos tok

beginString :: AlexInput -> Int -> Alex PosToken
beginString (p, _, _, _) _ = Alex (\s -> return (s{alex_ust=AlexUserState "" p}, ())) >> alexMonadScan

addCharToString :: Char -> AlexInput -> Int -> Alex PosToken
addCharToString c _ _ = Alex (\s@AlexState{alex_ust=AlexUserState str pos} -> return (s{alex_ust=AlexUserState (c:str) pos}, ())) >> alexMonadScan

addDecimalToString :: AlexInput -> Int -> Alex PosToken
addDecimalToString a@(_, _, _, inp) len
  | i < 256 = addCharToString (chr i) a len
  | otherwise = lexerError $ "Invalid ascii code: " ++ inp
  where i = fst $ head $ readDec $ drop 1 inp

addCurCharToString :: AlexInput -> Int -> Alex PosToken
addCurCharToString i@(_, _, _, s) = addCharToString (head s) i

endString :: AlexInput -> Int -> Alex PosToken
endString _ _ = Alex $ \s@AlexState{alex_ust=AlexUserState str pos} -> return $ (s, PosToken pos (StrTok $ reverse str))

alexEOF :: Alex PosToken
alexEOF = Alex $ \s@AlexState{alex_pos=pos} -> return $ (s, PosToken pos EOFTok)

data AlexUserState = AlexUserState String AlexPosn

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "" (AlexPn 0 0 0)

lexerError :: String -> Alex a
lexerError msg =
    do (p, c, _, inp) <- alexGetInput
       let inp1 = filter (/= '\r') (takeWhile (/='\n') inp)
       let inp2 = if (length inp1 > 30)
                     then trim (take 30 inp1)
                     else trim inp1
       let disp = if (null inp)
                     then " at end of file"
                     else if (null inp2)
                             then " before end of line"
                             else " on char " ++ show c ++ " before : '" ++ inp2 ++ "'"
       let disp3 = if (null msg)
                      then "Lexer error"
                      else trim msg
       alexError (disp3 ++ " at " ++ showPosn p ++ disp)
  where trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')
        showPosn (AlexPn _ line col) = show line ++ ':': show col

data PosToken = PosToken AlexPosn Token deriving Eq

instance Show PosToken where
    show (PosToken _ tok) = show tok

data Token = TypeTok
           | ArrayTok
           | OfTok
           | VarTok
           | NilTok
           | FunTok
           | LetTok
           | InTok
           | EndTok
           | IfTok
           | ThenTok
           | ElseTok
           | WhileTok
           | DoTok
           | ForTok
           | ToTok
           | BreakTok
           | ComaTok
           | ColonTok
           | SemicolonTok
           | DotTok
           | LParTok
           | RParTok
           | LBracketTok
           | RBracketTok
           | LBraceTok
           | RBraceTok
           | PlusTok
           | MinusTok
           | StarTok
           | SlashTok
           | AmpTok
           | VLineTok
           | EqTok
           | AssignTok
           | GtTok
           | GeTok
           | LtTok
           | LeTok
           | NeTok
           | IdTok String
           | IntTok Int
           | StrTok String
           | EOFTok
           deriving Eq

instance Show Token where
    show tok = case tok of
        TypeTok -> "type"
        ArrayTok -> "array"
        OfTok -> "of"
        VarTok -> "var"
        NilTok -> "nil"
        FunTok -> "function"
        LetTok -> "let"
        InTok -> "in"
        EndTok -> "end"
        IfTok -> "if"
        ThenTok -> "then"
        ElseTok -> "else"
        WhileTok -> "while"
        DoTok -> "do"
        ForTok -> "for"
        ToTok -> "to"
        BreakTok -> "break"
        ComaTok -> ","
        ColonTok -> ":"
        SemicolonTok -> ";"
        DotTok -> "."
        LParTok -> "("
        RParTok -> ")"
        LBracketTok -> "["
        RBracketTok -> "]"
        LBraceTok -> "{"
        RBraceTok -> "}"
        PlusTok -> "+"
        MinusTok -> "-"
        StarTok -> "*"
        SlashTok -> "/"
        AmpTok -> "&"
        VLineTok -> "|"
        EqTok -> "="
        AssignTok -> ":="
        GtTok -> ">"
        GeTok -> ">="
        LtTok -> "<"
        LeTok -> "<="
        NeTok -> "<>"
        IdTok _ -> "identifier"
        IntTok _ -> "number"
        StrTok _ -> "string"
        EOFTok -> "EOF"
}
