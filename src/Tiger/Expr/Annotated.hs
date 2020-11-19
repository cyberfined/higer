{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE PatternSynonyms   #-}

module Tiger.Expr.Annotated
    ( -- Data types
      SourcePos(..)
    , SrcSpan(..)
    , Ann(..)
    , AnnF
    , PosLValF
    , PosLVal
    , PosExprF
    , PosExpr

      -- patterns
    , pattern AnnE
    , pattern While_
    , pattern For_
    , pattern Let_

      -- functions
    , annToAnnF
    , stripAnnotation
    ) where

import Tiger.Expr.Types

import Data.Text
import Data.Function(on)
import Data.Functor.Compose
import Data.Fix

data SourcePos = SourcePos String Int Int deriving (Eq, Show)

instance Ord SourcePos where
    (SourcePos _ l1 c1) `compare` (SourcePos _ l2 c2) = (l1 `compare` l2) <> (c1 `compare` c2)

data SrcSpan = SrcSpan { spanBegin :: SourcePos
                       , spanEnd :: SourcePos
                       } deriving (Eq, Ord, Show)

instance Semigroup SrcSpan where
    s1 <> s2 = SrcSpan ((min `on` spanBegin) s1 s2) ((max `on` spanEnd) s1 s2)

data Ann ann a = Ann { annotation :: ann
                     , annotated :: a
                     } deriving (Functor, Foldable, Traversable)

type AnnF ann f = Compose (Ann ann) f

annToAnnF :: Ann ann (f (Fix (AnnF ann f))) -> Fix (AnnF ann f)
annToAnnF = Fix . Compose

stripAnnotation :: Functor f => Fix (AnnF ann f) -> Fix f
stripAnnotation = unfoldFix (annotated . getCompose . unFix)

type PosLValF d = AnnF SrcSpan (LValF d)

type PosLVal d = Fix (PosLValF d)

type PosExprF d = AnnF SrcSpan (ExprF d)

type PosExpr d = Fix (PosExprF d)

pattern AnnE :: ann -> f (Fix (AnnF ann f)) -> Fix (AnnF ann f)
pattern AnnE ann f = Fix (Compose (Ann ann f))

pattern While_ :: SrcSpan -> e -> e -> PosExprF d e
pattern While_ ann cond body = Compose (Ann ann (While cond body))

pattern For_ :: SrcSpan -> Text -> Bool -> e -> e -> e -> PosExprF d e
pattern For_  ann var esc from to body = Compose (Ann ann (For var esc from to body))

pattern Let_ :: SrcSpan -> [d e] -> e -> PosExprF d e
pattern Let_ ann decs exp = Compose (Ann ann (Let decs exp))
