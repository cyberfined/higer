module Tiger.TextUtils
    ( intercalate
    , stringBuilder
    , lazyStringBuilder
    ) where

import           Data.Text              (Text)
import           Data.Text.Lazy.Builder (Builder)

import qualified Data.Text              as Text
import qualified Data.Text.Lazy         as LazyText
import qualified Data.Text.Lazy.Builder as Builder

intercalate :: (a -> Builder) -> Builder -> [a] -> Builder
intercalate f d (x:xs@(_:_)) = f x <> d <> intercalate f d xs
intercalate f _ [x]          = f x
intercalate _ _ []           = ""

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
