import Data.Numbers.Primes -- primeFactors
import Data.Bits

main = do print euler622

euler622 :: Integer -- 3010983666182123972
euler622 = sum.remSort $ map ((+ 1).eval) goodBits

mapping = [3,3,5,5,7,11,13,31,41,151,331,1321,61] -- duplicate factors

goodBits :: [Integer]
goodBits = [n | n<-[1..2^13-1], bitCond n]
  where
    cond b = all (\c -> b .&. c /= b) 
    bitCond b | b <= 85   = cond b [1715,1714,1713,656,430,429,426,425,422,421,91,90,89,87,86,85]
              | b <= 86   = cond b [1715,1714,1713,656,430,429,426,425,422,421,91,90,89,87,86]
              | b <= 87   = cond b [1715,1714,1713,656,430,429,426,425,422,421,91,90,89,87]
              | b <= 89   = cond b [1715,1714,1713,656,430,429,426,425,422,421,91,90,89]
              | b <= 90   = cond b [1715,1714,1713,656,430,429,426,425,422,421,91,90]
              | b <= 91   = cond b [1715,1714,1713,656,430,429,426,425,422,421,91]
              | b <= 421  = cond b [1715,1714,1713,656,430,429,426,425,422,421]
              | b <= 422  = cond b [1715,1714,1713,656,430,429,426,425,422]
              | b <= 425  = cond b [1715,1714,1713,656,430,429,426,425]
              | b <= 426  = cond b [1715,1714,1713,656,430,429,426]
              | b <= 429  = cond b [1715,1714,1713,656,430,429]
              | b <= 430  = cond b [1715,1714,1713,656,430]
              | b <= 656  = cond b [1715,1714,1713,656]
              | b <= 1713 = cond b [1715,1714,1713]
              | b <= 1714 = cond b [1715,1714]
              | b <= 1715 = cond b [1715]
              | otherwise = True

listify 0 = [] 
listify n = mod n 2 :listify (div n 2)

eval bit = f (listify bit) mapping
  where
    f [] m  = 1
    f bs [] = 1
    f (b:bs) (m:ms) | b == 1 = m * (f bs ms)
                    | otherwise = f bs ms

remSort :: Ord a => [a] -> [a]
remSort [] = []
remSort [a] = [a]
remSort (a:as) = (remSort.smaller) (a:as) ++ [a] ++ (remSort.larger) (a:as)
  where
    smaller (x:xs) = filter (< x) xs
    larger (x:xs) = filter (> x) xs
