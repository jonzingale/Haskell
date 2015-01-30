module Shuffle (pmonicrandos, knuffle)where

import System.Random


----The Shuffle itself
knuffle :: (Ord a) =>[a]->[a]					
knuffle xs = (snd.unzip.qsort.zip ((pmonicrandos.length) xs)) xs

----Random
mkBlanket = mkStdGen
spitRandos n = randomRs (0,n) (mkBlanket 42) 

--birthdays an almost unique list of random numbers
pmonicrandos bs = take bs ((spitRandos.thrufloat yearbirth) bs) 
yearbirth r = (r-r^2)/(2*log(0.5)) -- r=23 , for fixed prob p=1/2

---Sort
qsort :: (Ord a) => [a]->[a]
qsort [] = []
qsort (x:xs) = qsort (less x xs) ++ [x] ++ qsort (more x xs)
 where
 	less a bs = [b|b<-bs,b<=a]
	more a bs = [b|b<-bs,b>a]
	
--some typehandling
thrufloat :: (RealFrac a, Integral b) => (a->a) -> b -> b
thrufloat f n = (floor.f.fromIntegral) n