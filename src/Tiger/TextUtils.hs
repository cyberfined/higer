module Tiger.TextUtils
    ( TextBuildable(..)
    , showNode
    , showLeaf
    , showNodeNames
    , intercalate
    , stringBuilder
    , lazyStringBuilder
    ) where

import           Data.Text              (Text)
import           Data.Text.Lazy.Builder (Builder)

import qualified Data.Text              as Text
import qualified Data.Text.Lazy         as LazyText
import qualified Data.Text.Lazy.Builder as Builder

class TextBuildable a where
    toTextBuilder :: a -> Builder

showNode :: Builder -> [Builder -> Builder -> Builder] -> Builder -> Builder -> Builder
showNode node = showNodeNames node . map (Nothing,)

showLeaf :: Builder -> Builder -> Builder -> Builder
showLeaf s pr _ = pr <> s <> "\n"

showNodeNames :: Builder
              -> [(Maybe Builder, Builder -> Builder -> Builder)]
              -> Builder -> Builder -> Builder
showNodeNames node xs pr cpr = pr <> node <> "\n" <> showXs pr cpr
  where showXs :: Builder -> Builder -> Builder
        showXs = snd (foldr (\a b -> (False, go a b)) (True, \_ _ -> "") xs)

        go :: (Maybe Builder, Builder -> Builder -> Builder)
           -> (Bool, Builder -> Builder -> Builder)
           -> Builder -> Builder -> Builder
        go (mName, pfunc) (isLast, fs) goPr goCpr
          | Just name <- mName
          = showName name <> rest
          | otherwise
          = pfunc newPr newCpr <> rest
          where rest = fs goPr goCpr
                (newPr, newCpr) = if isLast
                                     then (goCpr <> "└── ", goCpr <> "    ")
                                     else (goCpr <> "├── ", goCpr <> "│   ")
                showName name =  newPr <> name
                              <> "\n" <> pfunc (newCpr <> "└── ") (newCpr <> "    ")

intercalate :: Builder -> [Builder] -> Builder
intercalate d (x:xs@(_:_)) = x <> d <> intercalate d xs
intercalate _ [x]          = x
intercalate _ _            = ""

stringBuilder :: Text -> Builder
stringBuilder s = "\"" <> stringBuilder' s <> "\""
  where stringBuilder' :: Text -> Builder
        stringBuilder' str
            | Text.null str = ""
            | otherwise =
                let isSpec x = x == '\n' || x == '\t' || x == '\\' || x == '"'
                    prefix = Text.takeWhile (not . isSpec) str
                    prefixBuilder = Builder.fromText prefix
                    suffix = Text.drop (Text.length prefix) str
                    specBuilder :: Char -> Builder
                    specBuilder = \case
                        '\n' -> "\\n"
                        '\t' -> "\\t"
                        '\\' -> "\\\\"
                        _    -> "\""
                    specsText  = Text.takeWhile isSpec suffix
                    suffix' = Text.drop (Text.length specsText) suffix
                    specsBuilder = Text.foldr (\c a -> specBuilder c <> a) "" specsText
                    rest = stringBuilder' suffix'
                in prefixBuilder <> specsBuilder <> rest

lazyStringBuilder :: LazyText.Text -> Builder
lazyStringBuilder s = "\"" <> stringBuilder' s <> "\""
  where stringBuilder' :: LazyText.Text -> Builder
        stringBuilder' str
            | LazyText.null str = ""
            | otherwise =
                let isSpec x = x == '\n' || x == '\t' || x == '\\' || x == '"'
                    prefix = LazyText.takeWhile (not . isSpec) str
                    prefixBuilder = Builder.fromLazyText prefix
                    suffix = LazyText.drop (LazyText.length prefix) str
                    specBuilder :: Char -> Builder
                    specBuilder = \case
                        '\n' -> "\\n"
                        '\t' -> "\\t"
                        '\\' -> "\\\\"
                        _    -> "\""
                    specsText  = LazyText.takeWhile isSpec suffix
                    suffix' = LazyText.drop (LazyText.length specsText) suffix
                    specsBuilder = LazyText.foldr (\c a -> specBuilder c <> a) "" specsText
                    rest = stringBuilder' suffix'
                in prefixBuilder <> specsBuilder <> rest
