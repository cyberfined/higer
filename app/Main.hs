module Main (main) where

import Tiger.Expr
import Tiger.Parser (parse)

import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    src <- TIO.readFile "test.tig"
    case parse "test.tig" src of
        Left err   -> TIO.putStr err
        Right expr -> TIO.putStr $ exprToText expr
