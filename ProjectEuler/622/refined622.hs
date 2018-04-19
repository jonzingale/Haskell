module Refined622 where
import Math.NumberTheory.Primes -- factorise
import FactorisationHelpers
import Data.Bits

import Data.List

main = do print $ euler622 60

euler622 :: Integer -> Integer
euler622 n = sum [ k + 1 | k <- listDivisors (2^n-1), divCond n k]
  where
    divCond n k = and [ mod (2^t-1) k /= 0 | t <- properDivisors n]

-- Provables:
exactly8 = f 2
  where
    f n | n > 2^8 = []
        | riffleId n == 8 = n : f (n+2)
        | otherwise = f (n+2)

riffleId n = f (riffleOnce [1..n]) [1..n] 1
  where
    half as = div (length as) 2
    takeHalf as = [take (half as) as, drop (half as) as]
    riffleOnce = concat . transpose . takeHalf
    f j const k | j == const = k
                | otherwise = f (riffleOnce j) const (k+1) 