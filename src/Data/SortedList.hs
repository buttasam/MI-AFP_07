module Data.SortedList where

import Data.List (sort)
import Data.Semigroup

-- | List which is expected to be sorted
data SortedList a = a :<$ SortedList a
                  | Nil
                  deriving (Show, Read, Eq)

-- | Check validity of list (if is sorted)
-- DO NOT CHANGE THIS
valid :: Ord a => SortedList a -> Bool
valid Nil                   = True
valid (_ :<$ Nil)           = True
valid (x :<$ xs@(y :<$ ys)) = x < y && valid xs

-- | Construct sorted list from regular list
-- DO NOT CHANGE THIS
fromList :: Ord a => [a] -> SortedList a
fromList = foldr (:<$) Nil . sort

-- | Get the smallest element in sorted list
-- DO NOT CHANGE THIS
smallest :: Ord a => SortedList a -> a
smallest (x :<$ _) = x
smallest _ = error "SortedList: Nil"

instance Ord a => Semigroup (SortedList a) where
  -- | Merge two sorted lists
  -- DO NOT USE Data.List and its sort! it should be O(n+m) complexity
  (<>) l Nil = l
  (<>) Nil l = l
  (<>) l@(l1 :<$ xl) m@(m1 :<$ xm) = if l1 < m1 then l1 :<$ ((<>) xl m) else m1 :<$ ((<>) l xm)


instance Ord a => Monoid (SortedList a) where
  mempty = Nil
  mappend = (<>)

instance Functor SortedList where
  -- | Apply function over sorted list
  fmap = undefined

instance Applicative SortedList where
  pure  = undefined
  -- | Apply all functions to elements in sorted list
  (<*>) = undefined

instance Monad SortedList where
  -- | Apply on sorted list if valid and not empty
  (>>=)  = undefined
