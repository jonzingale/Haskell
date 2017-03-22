-- {-# OPTIONS_GHC -fno-warn-tabs #-}
module SortableTrees where
import Prelude hiding (head, (++), tail, length, filter)

t1 = Fork (Leaf 4) (Leaf 1)
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
  (++) Empty tree = tree
  (++) tree Empty = tree
  (++) left right = Fork left right
  tail Empty = Empty
  tail (Leaf a) = Empty
  tail (Fork _ r) = r
  head Empty = Empty
  head (Leaf a) = Leaf a
  head (Fork l _) = l
  unit = Empty

class (Ord s, Show s) => Sortable s where
  filter :: (s -> Bool) -> s -> s
  head, tail, qsort :: s -> s
  (++) :: s -> s -> s
  length :: s -> Int
  unit :: s

  length ns | ns == unit = 0
            | otherwise = 1 + (length.tail) ns

  filter bool ns = f bool ns unit
    where
      f bool js accum | js == unit = accum
                      | (bool.head) js = f bool (tail js) $  head js ++ accum
                      | otherwise = f bool (tail js) accum

  qsort ns | ns == unit = unit
           | otherwise = branch smaller ns ++ head ns ++ branch larger ns
    where
      branch f xs = qsort.f (head xs) $ tail xs
      smaller n ns = filter (<= n) ns
      larger n ns  = filter (>  n) ns