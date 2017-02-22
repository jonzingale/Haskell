module NumberArray where
import Prelude hiding (head, (++), tail, length, push)

-- Extend to a new TypeClass Naturals to protect bounds.

type N = Integer

head :: N -> N
head n = mod n 10

tail :: N -> N
tail n = div n 10

length :: N -> N
length 0 = 0
length n = 1 + (length.tail) n

(++) :: N -> N -> N
(++) n m = m + n*10^(length m)

push :: N -> N -> N
push n ns = n + ns * 10

smaller :: N -> N -> N
smaller n ns = s n ns 0
  where
    s 0 _ accum = accum
    s _ 0 accum = accum
    s j js accum | head js <= j = s j (tail js) (push (head js) accum)
                 | otherwise = s j (tail js) accum

larger :: N -> N -> N
larger n ns = l n ns 0
  where
    l 0 _ accum = accum
    l _ 0 accum = accum
    l j js accum | head js > j = l j (tail js) (push (head js) accum)
                 | otherwise = l j (tail js) accum

qsort :: N -> N
qsort 0 = 0
qsort ns = (qsort.smaller (head ns) $ tail ns) ++ (head ns) ++
           (qsort.larger  (head ns) $ tail ns)