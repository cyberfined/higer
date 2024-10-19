module Tiger.EnumMap
    ( EnumMap
    , empty
    , insert
    , lookup
    , (!)
    , update
    , alter
    , keys
    ) where

import           Data.IntMap.Strict (IntMap)
import           Prelude            hiding (lookup)

import qualified Data.IntMap.Strict as IntMap

newtype EnumMap k v = EnumMap { getEnumMap :: IntMap v }

empty :: Enum k => EnumMap k v
empty = EnumMap IntMap.empty
{-# INLINE empty #-}

insert :: Enum k => k -> v -> EnumMap k v -> EnumMap k v
insert k v = EnumMap . IntMap.insert (fromEnum k) v . getEnumMap
{-# INLINE insert #-}

lookup :: Enum k => k -> EnumMap k v -> Maybe v
lookup k = IntMap.lookup (fromEnum k) . getEnumMap
{-# INLINE lookup #-}

(!) :: Enum k => EnumMap k v -> k -> v
(!) m = (getEnumMap m IntMap.!) . fromEnum
{-# INLINE (!) #-}

update :: Enum k => (v -> Maybe v) -> k -> EnumMap k v -> EnumMap k v
update f k = EnumMap . IntMap.update f (fromEnum k) . getEnumMap
{-# INLINE update #-}

alter :: Enum k => (Maybe v -> Maybe v) -> k -> EnumMap k v -> EnumMap k v
alter f k = EnumMap . IntMap.alter f (fromEnum k) . getEnumMap
{-# INLINE alter #-}

keys :: Enum k => EnumMap k v -> [k]
keys = map toEnum . IntMap.keys . getEnumMap
{-# INLINE keys #-}
