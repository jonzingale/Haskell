module Sortable where
import Listables

class (Ord s, Listable s) => Sortable s where
  filterS :: (s -> Bool) -> s -> s
  qsort :: s -> s

  filterS b ns = f b ns unit
    where
      f b js accum | js == unit  = accum
                   | (b.headL) js = f b (tailL js) $ headL js +++ accum
                   | otherwise   = f b (tailL js) accum

  qsort ns | ns == unit = unit
           | otherwise = branch smaller ns +++ headL ns +++ branch larger ns
    where
      branch f xs = qsort.f (headL xs) $ tailL xs
      smaller n ns = filterS (<= n) ns
      larger n ns  = filterS (>  n) ns


instance Sortable Integer where
instance Ord a => Sortable [a] where
