module HighlyComposite where
import Data.List
import Primes

maxBy :: Ord a => (t -> a) -> [t] -> [t]
maxBy _ [] = []
maxBy _ [a] = [a]
maxBy c (n:ns:nss) | (c n) < (c ns) = maxBy c (ns:nss)
									 | otherwise = maxBy c (n:nss)

hi_comp :: Z -> Z
hi_comp n = head $ maxBy (size.ffactors) [1..n]

hi_cs = [(k,hi_comp k) | k <- [1..]]

foldrn xs = foldr (*) (head xs) (tail xs)

comb :: (Num a, Num a1, Ord a1) => a1 -> a1 -> a
comb _ 0 = 1
comb n k | k > n = 0
			   | otherwise = comb (n-1) k + comb (n-1) (k-1)

fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

tenFibs = (\n -> [(length.fibstr) x | x<-[1..n]]) 10
comblist n = (\n -> [comb n k | k <- [0..n]]) n
strlist n = foldr ((++).show) "" n


-- (\n -> [combstr n k | k <-[0..n+1]]) 4
-- ["1","01111","0010110111","0001001011","00001","0"]

{-- combstr 4 k
1
011111
001011011101111
00010010110010110111
000010001001011
000001
0
--}

-- 

-- string versions
combstr _ 0 = "1"
combstr n k | k > n = "0"
			  	  | otherwise = combstr (n-1) k ++ "*" ++ combstr (n-1) (k-1)

fibstr 0 = "0"
fibstr 1 = "1"
fibstr n = fibstr (n-2) ++ fibstr (n-1)


epis d c = sum [(-1)^k * comb c k * (c-k)^d| k<-[0..c]]
-- how might I do this recursively?
