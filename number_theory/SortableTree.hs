-- {-# OPTIONS_GHC -fno-warn-tabs #-}
module Sortable where
import Prelude hiding (head, (++), tail, length, filter)

ex_2 :: [Integer]
ex_2 = qsort [2,9,3,7,4,5,2,4,3,5,7,9,8]

t1 = Fork (Leaf 1) (Leaf 4)
t2 = Fork (Leaf 3) t1
t3 = t1 ++ t2

data Btree a = Leaf a | Fork (Btree a) (Btree a) | Empty
  deriving (Show , Eq)

instance Functor Btree where
  fmap f (Leaf n) = Leaf (f n)
  fmap f (Fork l r) = Fork (fmap f l) (fmap f r)

instance (Ord a, Show a) => Ord (Btree a) where
  (<=) t Empty = t == Empty
  (<=) (Leaf a) (Leaf b) = a <= b
  (<=) (Leaf a) (Fork l r) = (Leaf a) <= l
  (<=) (Fork l r) (Fork s t) = l <= s

instance (Ord a, Show a) => Sortable (Btree a) where
  (++) left right = Fork left right
  tail Empty = Empty
  tail (Leaf a) = Empty
  tail (Fork l r) = tail l
  unit = Empty
  head n = n

instance (Ord a, Show a) => Sortable [a] where
  (++) [] b = b
  (++) [a] [b] = [a,b]
  (++) (a:as) bs = a : (as ++ bs)
  head (n:ns) = [n]
  tail (n:ns) = ns
  unit = []

class (Ord s, Show s) => Sortable s where
  filter :: (s -> Bool) -> s -> s
  head, tail, qsort :: s -> s
  (++) :: s -> s -> s
  length :: s -> Int
  unit :: s

  length ns | ns == unit = 0 
            | otherwise = 1 + (length.tail) ns

  filter b ns = f b ns unit
    where
      f b js accum | js == unit = accum
                   | (b.head) js = f b (tail js) $  head js ++ accum
                   | otherwise   = f b (tail js) accum

  qsort ns | ns == unit = unit
           | otherwise = branch smaller ns ++ head ns ++ branch larger ns
    where
      branch f xs = qsort.f (head xs) $ tail xs
      smaller n ns = filter (<= n) ns
      larger n ns  = filter (>  n) ns