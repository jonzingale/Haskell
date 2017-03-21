-- {-# OPTIONS_GHC -fno-warn-tabs #-}
module Sortable where
import Prelude hiding (head, (++), tail, length, push, filter)
import Data.Tree

ex_1 :: Integer
ex_1 = qsort 2937452435798

ex_2 :: [Integer]
ex_2 = qsort [2,9,3,7,4,5,2,4,3,5,7,9,8]

instance Sortable Integer where
  (++) n m = m + n * 10^(length m)
  push n ns = n + ns * 10
  head n = mod n 10
  tail n = div n 10
  unit = 0

instance (Ord a, Show a) => Sortable [a] where
  (++) [] b = b
  (++) [a] [b] = [a,b]
  (++) (a:as) bs = a : (as ++ bs)
  push (a:[]) bs = a : bs
  head (n:ns) = [n]
  tail (n:ns) = ns
  unit = []

class (Ord s, Show s) => Sortable s where
  filter :: (s -> Bool) -> s -> s
  head, tail, qsort :: s -> s
  (++) :: s -> s -> s
  push :: s -> s -> s
  length :: s -> Int
  unit :: s

  length ns | ns == unit = 0 
            | otherwise = 1 + (length.tail) ns

  filter b ns = f b ns unit
    where
      f b js accum | js == unit = accum
                   | (b.head) js = f b (tail js) $ head js ++ accum
                   | otherwise   = f b (tail js) accum

  qsort ns | ns == unit = unit
           | otherwise = branch smaller ns ++ head ns ++ branch larger ns
    where
      branch f xs = qsort.f (head xs) $ tail xs
      smaller n ns = filter (<= n) ns
      larger n ns  = filter (>  n) ns