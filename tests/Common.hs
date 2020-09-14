module Common where

import Test.HUnit
import Data.Text(unpack)
import Tiger.Expr

newtype ShowUntypedExpr = ShowUntypedExpr {runShowUntypedExpr :: Expr UntypedDec} deriving Eq

newtype ShowTypedExpr = ShowTypedExpr {runShowTypedExpr :: Expr TypedDec} deriving Eq

instance Show ShowUntypedExpr where
    show = ('\n':) . unpack . showExpr . runShowUntypedExpr

instance Show ShowTypedExpr where
    show = ('\n':) . unpack . showExpr . runShowTypedExpr

mkTestLabel :: String -> [Assertion] -> Test
mkTestLabel lbl = TestLabel lbl . TestList . map TestCase
