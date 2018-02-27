module SortsShuffles where
import Data.List
import Typehandling
import System.Random

----The Shuffle itself
knuffle :: Ord a => [a] -> [a]
knuffle xs = (snd.unzip.qsort.zip ((pmonicrandos.length) xs)) xs

----Random
mkBlanket = mkStdGen
spitRandos n = randomRs (0,n) (mkBlanket 42) 

--birthdays an almost unique list of random numbers
pmonicrandos bs = take bs ((spitRandos.thrufloat yearbirth) bs) 

-- Feller's approximation to the standard birthday problem
probbirth n r = exp $(r-r^2)/(2*n) -- n =365, r =23
yearbirth r = (r-r^2)/(2*log(0.5)) -- r=23 , for fixed prob p=1/2
yrpeople n = half + sqrt (23*n/20) -- n =365, for fixed prob p=1/2

---Sorts
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort (less x xs) ++ [x] ++ qsort (more x xs)
 where
  less a bs = [b | b <- bs, b <= a]
  more a bs = [b | b <- bs, b > a]

rsort :: Ord a => [a]->[a]
rsort = qsort.knuffle

burrwheel :: Ord a=> [a] -> [a]
burrwheel xs = [a | (a:as) <- map reverse ((qsort.rots xs) 0)]
       where
       rots (x:xs) n| n==length (x:xs) = []
        | otherwise = (xs++[x]) : (rots (xs++[x]) (n+1))

        ----------------------
qsorty::Ord a => [a] -> [a]
qsorty [] = []
qsorty [x]=[x]
qsorty xs = (qsorty.less) xs ++ [head xs] ++ (qsorty.more) xs
    where
      less (x:xs) = [i |i<-xs , i < x]
      more (x:xs) = [i |i<-xs , i >= x]

msort:: Ord a => [a] -> [a]
msort [] =[]
msort [x] = [x]
msort (a:b:cs) = mstep (mstep [a] [b]) (msort cs)
 where
    mstep x [] = x
    mstep x (y:ys) | y >= maximum x = x ++ (y:ys)
                   | otherwise = mstep ((takeWhile (< y) x) ++ 
                                 [y] ++ (dropWhile (< y) x)) ys

