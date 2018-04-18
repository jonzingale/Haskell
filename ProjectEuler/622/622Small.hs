module Small622 where
import Math.NumberTheory.Primes -- factorise
import FactorisationHelpers
import Data.Bits

import Data.List

main = do print var622

var622 :: Integer
var622 = sum.goodBits $ 60

goodBits :: Integer -> [Integer]
goodBits n = [ k + 1 | k <- listDivisors (2^n-1), divCond n k]
  where
    divCond n k = allTrue [ mod (2^t-1) k /= 0 | t <- midFacts n]
    midFacts = tail.reverse.listDivisors
    allTrue = foldr (&&) True


-- goodBits :: [Integer]
-- goodBits = [n | n<-[1..2^3-1], bitCond n] -- 3 because 3 unique factors
-- -- goodBits = [n | n<-[1..2^13-1], bitCond n]
--   where
--     -- bitCond b = cond b [1715,1714,1713,656,430,429,426,425,422,421,91,90,89,87,86,85]
--     cond b = all (\c -> b .&. c /= b)
--     bitCond b = cond b [1,2,3]

bitify [] = 0
bitify (n:ns) = n + bitify (map (* 2) ns)

listify 0 = [] 
listify n = mod n 2 :listify (div n 2)

-- eval bit = f (listify bit) mapping
--   where
--     f [] m  = 1
--     f bs [] = 1
--     f (b:bs) (m:ms) | b == 1 = m * (f bs ms)
--                     | otherwise = f bs ms

remSort :: Ord a => [a] -> [a]
remSort [] = []
remSort [a] = [a]
remSort (a:as) = (remSort.smaller) (a:as) ++ [a] ++ (remSort.larger) (a:as)
  where
    smaller (x:xs) = filter (< x) xs
    larger (x:xs) = filter (> x) xs


-- Provables:
sixties = f 2
  where
    f n | n > 2^15 = []
        | riffleId n == 15 = n : f (n+2)
        | otherwise = f (n+2)

riffleId n = f (riffleOnce [1..n]) [1..n] 1
  where
    half as = div (length as) 2
    takeHalf as = [take (half as) as, drop (half as) as]
    riffleOnce = concat . transpose . takeHalf
    f j const k | j == const = k
                | otherwise = f (riffleOnce j) const (k+1) 