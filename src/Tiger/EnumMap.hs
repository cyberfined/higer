module Tiger.EnumMap
    ( EnumMap
    , toIntMap
    , empty
    , insert
    , lookup
    , (!)
    , update
    , delete
    , alter
    , keys
    , fromList
    , toList
    , withoutKeys
    , foldlWithKey
    ) where

import           Data.Bifunctor     (first)
import           Data.Function      (on)
import           Data.IntMap.Strict (IntMap)
import           Prelude            hiding (lookup)

import           Tiger.EnumSet      (EnumSet, toIntSet)

import qualified Data.IntMap.Strict as IntMap

newtype EnumMap k v = EnumMap { toIntMap :: IntMap v }

instance (Enum k, Eq v) => Eq (EnumMap k v) where
  (==) = (==) `on` toIntMap

empty :: Enum k => EnumMap k v
empty = EnumMap IntMap.empty
{-# INLINE empty #-}

insert :: Enum k => k -> v -> EnumMap k v -> EnumMap k v
insert k v = EnumMap . IntMap.insert (fromEnum k) v . toIntMap
{-# INLINE insert #-}

lookup :: Enum k => k -> EnumMap k v -> Maybe v
lookup k = IntMap.lookup (fromEnum k) . toIntMap
{-# INLINE lookup #-}

(!) :: Enum k => EnumMap k v -> k -> v
(!) m = (toIntMap m IntMap.!) . fromEnum
{-# INLINE (!) #-}

update :: Enum k => (v -> Maybe v) -> k -> EnumMap k v -> EnumMap k v
update f k = EnumMap . IntMap.update f (fromEnum k) . toIntMap
{-# INLINE update #-}

delete :: Enum k => k -> EnumMap k v -> EnumMap k v
delete k = EnumMap . IntMap.delete (fromEnum k) . toIntMap
{-# INLINE delete #-}

alter :: Enum k => (Maybe v -> Maybe v) -> k -> EnumMap k v -> EnumMap k v
alter f k = EnumMap . IntMap.alter f (fromEnum k) . toIntMap
{-# INLINE alter #-}

keys :: Enum k => EnumMap k v -> [k]
keys = map toEnum . IntMap.keys . toIntMap
{-# INLINE keys #-}

fromList :: Enum k => [(k, v)] -> EnumMap k v
fromList = EnumMap . IntMap.fromList . map (first fromEnum)
{-# INLINE fromList #-}

toList :: Enum k => EnumMap k v -> [(k, v)]
toList = map (first toEnum) . IntMap.toList . toIntMap
{-# INLINE toList #-}

withoutKeys :: Enum k => EnumMap k v -> EnumSet k -> EnumMap k v
withoutKeys enMap enSet = EnumMap $ IntMap.withoutKeys (toIntMap enMap) (toIntSet enSet)
{-# INLINE withoutKeys #-}

foldlWithKey :: Enum k => (a -> k -> v -> a) -> a -> EnumMap k v -> a
foldlWithKey f ini = IntMap.foldlWithKey (\acc k -> f acc (toEnum k)) ini . toIntMap
{-# INLINE foldlWithKey #-}
