module Tiger.Utils where

import Data.Fix
import Data.Text
import Control.Monad.State

showText :: Show a => a -> Text
showText = pack . show

mapAccumM :: (Monad m, Functor m, Traversable t) => (a -> b -> m (c, a)) -> a -> t b -> m (t c)
mapAccumM f = flip (evalStateT . traverse (StateT . flip f))

adi :: Functor f => (f a -> a) -> ((Fix f -> a) -> Fix f -> a) -> Fix f -> a
adi f g = g (f . fmap (adi f g) . unFix)
