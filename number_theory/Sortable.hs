{-# OPTIONS_GHC -fno-warn-tabs #-}
-- # LANGUAGE FlexibleInstances #

module Sortable where
import Prelude hiding (head, (++), tail, length, push, filter)
import Data.Monoid

instance Monoid Integer where
  mconcat = foldr mappend mempty
  mappend a b = a + b
  mempty = 0

instance ArrayLike Integer where
  head n = mod n 10
  tail n = div n 10
  length 0 = 0
  length n = 1 + (length.tail) n
  (++) n m = m + n * 10^(length m)
  push n ns = n + ns * 10
  filter b ns = f b ns 0
    where
      f b 0 accum  = accum
      f b js accum | (b.head) js = f b (tail js) $ push (head js) accum
                   | otherwise   = f b (tail js) accum

-- instance ArrayLike s where
--   qsort mempty = mempty
--   qsort ns = branch smaller ns ++ head ns ++ branch larger ns
--     where
--       branch f xs = qsort.f (head xs) $ tail xs
--       smaller n ns = filter (<= n) ns
--       larger n ns  = filter (>  n) ns

class (Ord s, Eq s, Show s, Monoid s) => ArrayLike s where
  head :: s -> s
  tail :: s -> s
  length :: s -> Int
  (++) :: s -> s -> s
  push :: s -> s -> s
  filter :: (s -> Bool) -> s -> s
  qsort :: s -> s

-- class (Ord s, ArrayLike s) => Sortable s where
--   qsort :: s -> s
