module Tiger.EnumSet
    ( EnumSet
    , toIntSet
    , empty
    , insert
    , delete
    , member
    , null
    , size
    , union
    , difference
    , (\\)
    , toList
    , fromList
    , foldl
    , foldr
    ) where

import           Data.IntSet (IntSet)
import           Prelude     hiding (foldl, foldr, null)

import qualified Data.IntSet as IntSet

newtype EnumSet a = EnumSet
    { toIntSet :: IntSet
    } deriving newtype Eq

empty :: Enum a => EnumSet a
empty = EnumSet IntSet.empty
{-# INLINE empty #-}

insert :: Enum a => a -> EnumSet a -> EnumSet a
insert x = EnumSet . IntSet.insert (fromEnum x) . toIntSet
{-# INLINE insert #-}

delete :: Enum a => a -> EnumSet a -> EnumSet a
delete x = EnumSet . IntSet.delete (fromEnum x) . toIntSet
{-# INLINE delete #-}

member :: Enum a => a -> EnumSet a -> Bool
member x = IntSet.member (fromEnum x) . toIntSet
{-# INLINE member #-}

null :: Enum a => EnumSet a -> Bool
null = IntSet.null . toIntSet
{-# INLINE null #-}

size :: Enum a => EnumSet a -> Int
size = IntSet.size . toIntSet
{-# INLINE size #-}

union :: Enum a => EnumSet a -> EnumSet a -> EnumSet a
union (EnumSet xs) (EnumSet ys) = EnumSet $ IntSet.union xs ys
{-# INLINE union #-}

difference :: Enum a => EnumSet a -> EnumSet a -> EnumSet a
difference (EnumSet xs) (EnumSet ys) = EnumSet $ IntSet.difference xs ys
{-# INLINE difference #-}

(\\) :: Enum a => EnumSet a -> EnumSet a -> EnumSet a
(\\) = difference
{-# INLINE (\\) #-}

toList :: Enum a => EnumSet a -> [a]
toList = map toEnum . IntSet.toList . toIntSet
{-# INLINE toList #-}

fromList :: Enum a => [a] -> EnumSet a
fromList = EnumSet . IntSet.fromList . map fromEnum
{-# INLINE fromList #-}

foldl :: Enum a => (b -> a -> b) -> b -> EnumSet a -> b
foldl f ini = IntSet.foldl (\acc -> f acc . toEnum) ini . toIntSet
{-# INLINE foldl #-}

foldr :: Enum a => (a -> b -> b) -> b -> EnumSet a -> b
foldr f ini = IntSet.foldr (f . toEnum) ini . toIntSet
{-# INLINE foldr #-}
