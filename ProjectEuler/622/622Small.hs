module Small622 where
import Math.NumberTheory.Primes -- factorise
import Data.Bits

main = do print var622

-- var622 :: Integer -- 32767 == 2^15-1
-- var622 = sum.remSort $ map ((+ 1).eval) goodBits
var622 = remSort $ map ((+ 1).eval) goodBits

-- mapping = [3,3,5,5,7,11,13,31,41,151,331,1321,61] -- duplicate factors
mapping = [7,31,151] -- 2^15-1
-- mapping = [3,5,17] -- 2^8-1


goodBits :: [Integer]
goodBits = [n | n<-[1..2^3-1], bitCond n] -- 3 because 3 unique factors
-- goodBits = [n | n<-[1..2^13-1], bitCond n]
  where
    -- bitCond b = cond b [1715,1714,1713,656,430,429,426,425,422,421,91,90,89,87,86,85]
    cond b = all (\c -> b .&. c /= b)
    bitCond b = cond b [1,2,3]
    -- bitCond b = cond 

bitify [] = 0
bitify (n:ns) = n + bitify (map (* 2) ns)

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
