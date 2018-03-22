
{-# OPTIONS_GHC -Wno-missing-methods #-}

module BestHorse where
import qualified Numeric.LinearAlgebra.HMatrix as H
import Data.Numbers.Primes -- primeFactors
import Text.Printf
import Data.Bits

-- import Data.Set

two60Minus1 = 1152921504606846975
smallsMinus1 = [1073741823, 1048575, 32767, 4095] -- [2^i-1| i<-[30,20,15,12]]
smallLimit = 1073741823

{--
Scales linearly by tens, I need 10^18 which is ~ 3000 years
10^2 => 62
10^3 => 11528
10^4 => 400002
10^5 => 8232188
10^6 => 135411214 -- 0.48 secs
10^7 => 1895683502 -- 4.81 secs
10^8 => 23053423296 -- 47.25 secs

--}

testable :: Integer
testable = f 2 0 -- == 135411214
  where
    f 1000000 acum = acum
    f n acum | gg' n && ff' n = f (n+2) (n+acum)
             | otherwise = f (n+2) acum

    ff' n | n > smallLimit = True
          | otherwise = and [mod i (n-1) /= 0 | i <- smallsMinus1] 
    gg' n = mod two60Minus1 (n-1) == 0

-- BitWise
-- 4163905171863765444
euler622 :: Integer
euler622 = (+ 2^60).sum.remSort $ map ((+ 1).eval) goodBits
-- euler622' = sum.remSort $ map ((+ 1).eval) goodBits

mapping = [3,3,5,5,7,11,13,31,41,151,331,1321,61] -- duplicate factors

goodBits :: [Integer]
goodBits = [n | n<-[1..2^13-1], bitCond n]
  where
    cond b = all (\c -> b .&. c /= b) 
    bitCond b | b < 85   = cond b [1715,1714,1713,656,430,429,426,425,422,421,91,90,89,87,86,85]
              | b < 86   = cond b [1715,1714,1713,656,430,429,426,425,422,421,91,90,89,87,86]
              | b < 87   = cond b [1715,1714,1713,656,430,429,426,425,422,421,91,90,89,87]
              | b < 89   = cond b [1715,1714,1713,656,430,429,426,425,422,421,91,90,89]
              | b < 90   = cond b [1715,1714,1713,656,430,429,426,425,422,421,91,90]
              | b < 91   = cond b [1715,1714,1713,656,430,429,426,425,422,421,91]
              | b < 421  = cond b [1715,1714,1713,656,430,429,426,425,422,421]
              | b < 422  = cond b [1715,1714,1713,656,430,429,426,425,422]
              | b < 425  = cond b [1715,1714,1713,656,430,429,426,425]
              | b < 426  = cond b [1715,1714,1713,656,430,429,426]
              | b < 429  = cond b [1715,1714,1713,656,430,429]
              | b < 430  = cond b [1715,1714,1713,656,430]
              | b < 656  = cond b [1715,1714,1713,656]
              | b < 1713 = cond b [1715,1714,1713]
              | b < 1714 = cond b [1715,1714]
              | b < 1715 = cond b [1715]
              | otherwise = True

bitify [] = 0
bitify (n:ns) = n + bitify (map (* 2) ns)

listify 0 = [] 
listify n = mod n 2 :listify (div n 2)

eval bit = f (listify bit) mapping  -- verify these.
  where
    f [] m  = 1
    f bs [] = 1
    f (b:bs) (m:ms) | b == 1 = m * (f bs ms)
                    | otherwise = f bs ms

v1   = [1,1,0,0,1,1,0,1,0,1,1,0,0] -- duplicated [3,3,5,5,7,11]...
v1'  = [1,0,0,0,1,1,0,1,0,1,1,0,0] 
v1'' = [0,1,0,0,1,1,0,1,0,1,1,0,0]

v2   = [1,0,1,1,0,1,0,1,1,0,0,0,0]
v2'  = [1,0,0,1,0,1,0,1,1,0,0,0,0]
v2'' = [1,0,1,0,0,1,0,1,1,0,0,0,0]

w2   = [0,1,1,1,0,1,0,1,1,0,0,0,0]
w2'  = [0,1,0,1,0,1,0,1,1,0,0,0,0]
w2'' = [0,1,1,0,0,1,0,1,1,0,0,0,0]

v3   = [0,0,0,0,1,0,0,1,0,1,0,0,0]

v4   = [1,1,1,0,1,0,1,0,0,0,0,0,0]
v4'  = [1,0,1,0,1,0,1,0,0,0,0,0,0]
v4'' = [0,1,1,0,1,0,1,0,0,0,0,0,0]

w4   = [1,1,0,1,1,0,1,0,0,0,0,0,0]
w4'  = [0,1,0,1,1,0,1,0,0,0,0,0,0]
w4'' = [1,0,0,1,1,0,1,0,0,0,0,0,0]




vals = map bitify [v1, v2, v3, v4, v1',v1'',v2',v2'',v4',v4'', w2, w2',w2'',w4,w4',w4'']

instance Num a => Num [a] where
  (*) as [] = []
  (*) [] as = []
  (*) (a:as) (b:bs) = a*b : as * bs

remSort :: Ord a => [a] -> [a]
remSort [] = []
remSort [a] = [a]
remSort (a:as) = (remSort.smaller) (a:as) ++ [a] ++ (remSort.larger) (a:as)
  where
    smaller (x:xs) = filter (< x) xs
    larger (x:xs) = filter (> x) xs


main = do
  let format = "Test Match 135411214 => %s\nReal Deal: %i\n"
  putStr $ printf format (show testable) euler622
