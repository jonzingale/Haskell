
module Pprimes (mkBlanket,pPrime,spit,hardSpitter,fprimes,wran,cls,pprime,walk) where
import System.Random
import Typehandling

type Z = Integer
type Prime = Integer
type Base = Z
type Rand = Z

{--
There should be a way to utilize the 
symmetry of the witness list. It turns
out that the witness list is palindromic
and so only half are necessary to verify
that a number is or is not prime. Attempts
I have made recently have not been very
invested, but a more dedicated party
could certainly find a way to benefit.
--}

{--clears the screen--}
cls :: IO()
cls = putStr "\ESC[2J"

intToFloat :: Integer -> Float
intToFloat n = fromInteger (toInteger n)

{-- Spits verified primes --}
fprimes :: [Prime]
fprimes = [p | p<-[2..],
  let xs = [1..(floor.sqrt.intToFloat) p] in 
   [ x  | x <- xs, p`mod`x == 0 ] == [1] ]


{--Give it a tolerance and watch as it spits probable primes hard--}
spit :: Int->[Prime]
spit rhyme = [ prime | prime <- [2..] , pPrime prime rhyme]

{--A Hard spitting probable prime checker.
overtakes fprimes as a spitter around 380500.--}
pPrime :: Prime -> Int -> Bool 
pPrime toni tol = gza toni toni "tone" 
	where
	gza s p l
	  | kimchi ((s+2) `mod` p) p == False = False
	  | length l > tol = True
	  | otherwise = gza (wran s) p ('a':l)
          where kimchi a p = rmod a p == a `mod` p

-- what if instead of wran I use a variation on bday in pPrime?
-- randoms will only give at most 11 digit numbers


{--divides and conquers--}
rmod :: Z->Z->Z 
rmod a p = figit a p
 where figit b q =
	if (length.show) q > 4
	then let d = q`div`4 in
	     let r = q`mod`4 in
	      ((figit a d)^4)*((figit a r)) `mod` p
	else a^q`mod`p

{--random number mother--}
wran ::  Z -> Z
wran snug = (toInteger.fst.next.mkBlanket.fromIntegral) snug

--for spitting way larger randos
-- how can i write bday to reflect word size of number?
-- wwran :: (Num a,Read a,Show a)=> Int->Int->a
-- wwran s n = read $ take n $ (+++) [show i|i<-randomRs(0,9) (mkBlanket s)]
-- (+++) :: [[Char]] -> String
-- (+++) [] = " "
-- (+++) (a:as) = a++((+++)as)

mkBlanket :: Int -> StdGen
mkBlanket cozy = mkStdGen cozy

prime p = let xs = [1..(floor.sqrt.intToFloat) p] in 
   [ x  | x <- xs, p`mod`x == 0 ] == [1] 


pbjfactors :: Z->[Z] --a factorer based on probable primes
pbjfactors n = [ p | p<-takeWhile (< n) (spit 3), n`mod`p==0 ]   





------------------------- Birthdays and pPrimes

qsort [] = []
qsort (x:[]) = [x]
qsort x = (qsort(less x)) ++ [head x] ++ (qsort(more x))
 where 
   less xs = [as|as<-xs, as< head xs]
   more xs = [zs|zs<-xs, zs> head xs]

pShuff =  (\j-> (\ i-> take j [a|a<-randomRs (1,bday i) (mkBlanket 42)]) j)
sShuff = (qsort.pShuff)
-- We only need to consider witnesses 2..p/2)
pprime tol p | p < 2 = False
	     | p == 2 = True
	     | otherwise = let pp = p`div`2 in
	        let witnesses = [((ape p w) `rmod` p)  == (ape p w) | w <-randomRs (1,bday pp) (mkBlanket 42), ape p w < p] in
		--let witnesset =
  			(and.take tol) witnesses
			--(and.take tol) [w|(w,z)<-zip witnesset witnesses,w==z]
hardSpitter = [n | n<-walk, pprime 5 n ]

--Is it faster to run pprime k times on different seeds or
--to increase the tolerance by n ?


--helpers
bday r = thrufloat (\r->(r-r^2)/(2 * log 0.5)) r
ct x = (fromIntegral x) / ( (fromIntegral.bday) x )
ape p = thrufloat (* ((ct.fromIntegral) p))

walk = [1..]










