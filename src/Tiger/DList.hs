module Tiger.DList
    ( DList
    , toList
    , fromList
    , apply
    , empty
    , singleton
    , cons
    , (<|)
    , snoc
    , (|>)
    ) where

newtype DList a = DList { runDList :: [a] -> [a] }

toList :: DList a -> [a]
toList = flip runDList []
{-# INLINE toList #-}

fromList :: [a] -> DList a
fromList xs =  DList (xs++)
{-# INLINE fromList #-}

apply :: DList a -> [a] -> [a]
apply = runDList
{-# INLINE apply #-}

empty :: DList a
empty = DList id
{-# INLINE empty #-}

singleton :: a -> DList a
singleton x = DList (x:)
{-# INLINE singleton #-}

cons :: a -> DList a -> DList a
cons x (DList xs) = DList ((x:) . xs)
{-# INLINE cons #-}

infixl 5 <|, |>

(<|) :: a -> DList a -> DList a
(<|) = cons
{-# INLINE (<|) #-}

snoc :: DList a -> a -> DList a
snoc (DList xs) x = DList (xs . (++[x]))
{-# INLINE snoc #-}

(|>) :: DList a -> a -> DList a
(|>) = snoc
{-# INLINE (|>) #-}

instance Semigroup (DList a) where
    DList xs <> DList ys = DList (xs . ys)
    {-# INLINE (<>) #-}

instance Monoid (DList a) where
    mempty = empty
    {-# INLINE mempty #-}

instance Foldable DList where
    foldr f ini = foldr f ini . toList
    {-# INLINE foldr #-}

instance Functor DList where
    fmap f xs = DList (fmap f (toList xs)++)
    {-# INLINE fmap #-}

instance Show a => Show (DList a) where
    show = show . toList
    {-# INLINE show #-}
