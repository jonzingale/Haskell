module Junk where

type Z = Integer
type Prime = Integer

eratosthenes2 :: [Integer]
eratosthenes2 = 2 : sieveP [1..] 2
  where
    sieveP (n:ns) xs | gg (2 * n + 1) xs = sieveP ns xs
                     | otherwise = (2 * n + 1) : sieveP ns ((2 * n + 1)*xs)
    gg n m = gcd n m > 1

spiral :: Integer -> Integer
spiral n = ff n 1 5*10^6
  where
    ff n a 0 = 1
    ff n a c = a + n + ff (n + 8) (a + n) (c - 1)

totals = sum (map (spiral) [2,4,6,8]) - 3


-- deci :: Integer -> Integer
deci n = (n, div (10^100) n)


psinnum :: Integer -> [(Integer, Integer)]
psinnum n = ff n eratosthenes2 0
    where
        ff n (k:ks) j | n < k = [(k, j)]
                      | mod n k == 0 = ff (div n k) (k:ks) (j + 1)
                      | j == 0 = ff n ks 0
                      | otherwise = (k, j) :  ff n ks 0

psdivnum :: Integer -> [Integer]
psdivnum n = ff n eratosthenes2
    where
        ff n (k:ks) | n < k = []
                    | mod n k == 0 = k : ff (div n k) ks
                    | otherwise =  ff n ks

totient :: Integer -> Integer
totient n = ff.psinnum $ n
  where
    ff [] = 1
    ff ((p,j):zs) = p^(j-1) * (p-1) * ff zs

reltotient :: Integer -> Integer -> Integer
reltotient n m = gcd (totient n) (totient m)
-- 5882352941176470

prinny =   (putStr.unlines.map show) $ map deci eratosthenes2

-- cycleD :: Integer -> Integer
-- cycleD n = cc n []
--   where
--     cc _ 0 = 0
--     cc m as | 