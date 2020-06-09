module Arithmetic where
import System.Random
import SortsShuffles
import Primes

type N = Integer
cls = putStr "\ESC[2J"
{--
The number of divisors of n
The sum of the divisors of n
The sum of the divisors of n each to the kth power
The number of distinct primes dividing n
The total number of times primes divide n
The Mobius function mu
Is a number squarefree?
The Euler Totient function
Multiplicativity test (experimental)
--}
ofdivisors :: N->N   
ofdivisors n = (size.ffactors) n

sumofdivisors :: N->N
sumofdivisors n = (sum.ffactors) n

sumkdivisors :: N->N->N
sumkdivisors n k = sum [i^k|i<-ffactors n]

distinctp :: N->N
distinctp n = sum [ 1 | f <-ffactors n,prime f]

omegap ::N->N
omegap n = sum [fpsinnum n p|p<-(takeWhile (<= rootn n) fprimes)]

mu :: N->N
mu n    | sqrfree n = (-1)^(distinctp n)
	| otherwise = 0

sqrfree :: N->Bool
sqrfree n = distinctp n == omegap n

totient :: N->N
totient n = sum[1 |k<-[1..n], gcd n k==1]

multip :: (N->N)->Int->Bool
multip f seed= and[(f a)*(f b)==f(a*b)|(a,b)<-rcoprimes seed]


------helpers
dgnl :: a->(a,a)
dgnl a = (a,a)
rootn :: N->N
rootn n = root n+1
boredoms :: Int -> [Int]
boredoms zola = take zola (randoms (mkBlanket zola))
shuffle :: Ord a => [a]->[a]
shuffle xs = (snd.unzip.qsort)(zip ((boredoms.length) xs) xs)
tryagain :: Int -> [Int] --Random sorted listing of [1..n]
tryagain fool = (snd.unzip.qsort) (zip (boredoms fool) [1..fool]) 
rcoprimes :: Int-> [(Z,Z)] --Every fst is coprime to every snd.
rcoprimes n = let exp= map fromIntegral (tryagain 3++((shuffle.tryagain)3)) in
	      let (rs,qs)  =(((partition n).shuffle.take (n*5)) fprimes) in
	 zip [r^e|(r,e)<-zip rs exp]
             [q^f|(q,f)<-zip qs exp]
partition :: Int->[a]->([a],[a])
partition n xs  | n<=0 =([],xs) 
		| n>=(length xs) = (xs,[])
		| otherwise = (take n xs,drop n xs)
