module NumberArray where
import Prelude hiding (head, (++), tail, length, push, filter)

data NumArray n = N n | BAD deriving (Show, Eq, Ord)

incl :: (Integral a) => a -> NumArray a
incl n | n < 0 = BAD
       | otherwise = N n

eval :: (Integral a) => NumArray a -> a
eval (N a) = a
eval BAD = -1

instance (Num a, Integral a) => Num (NumArray a) where
  x + y | x == BAD || y == BAD = BAD
        | otherwise = incl (eval x + eval y)

  x * y | x == BAD || y == BAD = BAD
        | otherwise = incl (eval x * eval y)

  x - y | x == BAD || y == BAD = BAD
        | otherwise = incl (eval x - eval y)

-- instance (Num a, Integral a) => Integral (NumArray a) where
--   div x y | x == BAD || y == BAD = BAD
--           | otherwise = N (mod x y)

-- type N = NumArray Integer
type N = Integer

head :: N -> N
head n = mod n 10

tail :: N -> N
tail n = div n 10

length :: N -> N
length 0 = 0
length n = 1 + (length.tail) n

(++) :: N -> N -> N
(++) n m = m + n * 10^(length m)

push :: N -> N -> N
push n ns = n + ns * 10

filter :: (N -> Bool) -> N -> N
filter b ns = f b ns 0
  where
    f b 0 accum  = accum
    f b js accum | (b.head) js = f b (tail js) $ push (head js) accum
                 | otherwise   = f b (tail js) accum

qsort :: N -> N
qsort 0 = 0
qsort ns = (qsort.smaller (head ns) $ tail ns) ++ (head ns) ++
           (qsort.larger  (head ns) $ tail ns)
  where
    smaller n ns = filter (<= n) ns
    larger n ns  = filter (>  n) ns