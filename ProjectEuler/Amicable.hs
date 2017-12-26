-- # MINIMAL -Wmissing-methods #

module Amicable where
import Junk
import Triangles




euler21 :: Integer -> Integer
euler21 n | n > 10^4 = 0
          | amicable n = n + euler21 (n+1)
          | otherwise = euler21 (n+1)

amicable :: Integer -> Bool
amicable n | plusfacts n == n = False
           | otherwise = (plusfacts.plusfacts) n == n

plusfacts :: Integer -> Integer
plusfacts 1 = 1
plusfacts n = ff 0 1 n
  where
    ff accum x n | n == x = accum
                 | mod n x == 0 = ff (accum + x) (x + 1) n
                 | otherwise = ff accum (x + 1) n


-- eliminate all finites. mixtures of powers of 2 and 5.
-- find longest repeating.
allDigits = [div (10^10^3) k | k<-[1..1000]]

cycleP :: Integer -> Integer
cycleP n = ff n 1
  where
    ff n k | mod n 2 == 0 = ff (div n 2) k
           | mod n 5 == 0 = ff (div n 5) k
           | mod (10^k) n == mod (10^(k+1)) n = 1
           | mod (10^k) n == 1 = k
           | otherwise = ff n (k + 1)

euler26 :: Integer
euler26 = snd.maximum $ [ (cycleP k, k) | k <- takeWhile (< 10^3) eratosthenes2]


period n = head $ [ p | p <- [1..], (10^p - 1) `mod` n == 0 ]


