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


-- Euler 102
-- [[(448,617),(-988,0),(-103,-504)]] has a zero.

euler102 = head triangles

tri = T (V 4 1) (V (-3) (-5)) (V (-3) 6)
pt = V 2 4

data Vect = V Integer Integer
data Triangle = T Vect Vect Vect
type Point = Vect

(+|), (-|) :: Vect -> Vect -> Vect
V a b +| V c d = V (a+c) (b+d)
V a b -| V c d = V (a-c) (b-d)

innerP :: Vect -> Vect -> Integer
innerP (V a b) (V c d) = a*c + b*c

orth :: Vect -> Vect
orth (V x y) = V (-y) x

in_region :: Triangle -> Vect -> Bool
in_region (T a b c) pt =
  let acute = if condo (b -| a) (a -| c) then and else or in
  acute [condo pt (b -| a), condo pt (a -| c)]
  where
    condo v w = innerP v (orth w) >= 0

rotateT :: Triangle -> [Triangle]
rotateT (T a b c) = (T a b c) : rotateT (T b c a)





