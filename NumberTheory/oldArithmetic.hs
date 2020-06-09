module Arithmetic (pollard, pollaria)where
import System.Random
import SortsShuffles

import Primes

-- mkBlanket :: Int -> StdGen
-- mkBlanket cozy = mkStdGen cozy

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
--
mu2: another mobius
sqfree: a faster square free checker
--
The Euler Totient function
phi: totienting from a prime factorization<--much faster than totient
totphi: another method of totient
tot: and another totient
Multiplicativity test (experimental)
Mobius Inversion (an Attempt)
mu's under the n
inxs : Inclusion-Exclusion
neatfac: a clean way to prime factor
pano: gives the largest power divisible by a given number in another
----
Elevens trick as a Blinky light

bk: gives the next state
blink: gives the next cell for a particular index
beta & wick: Provide the blinky 11 rule

list2N: takes a list of numbers and returns a string-like number
n2List: takes a number and returns it as a list

lenN: takes the length of a number
lminus: take the length of a number n, then subtract i

onesN: take the length of a number n and return the number described by n ones
-----------
pollard: pollard's factor finder in (sqrt p) time.

--}
ofdivisors :: N->N   
ofdivisors n = (size.ffactors) n

sumofdivisors :: N->N
sumofdivisors n = (sum.ffactors) n

sumkdivisors :: N->N->N
sumkdivisors n k = sum [i^k|i<-ffactors n]

distinctp :: N->N
distinctp n = sum [ 1 | f<-ffactors n,prime f]

omegap ::N->N
omegap n = sum [fpsinnum n p|p<-(takeWhile (<= rootn n) fprimes)]

mu :: N->N
mu n    | sqrfree n = (-1)^(distinctp n)
  | otherwise = 0

sqrfree :: N->Bool
sqrfree n = distinctp n == omegap n
---
mu2 :: N->N
mu2 n| (not.sqfree) n = 0
     | (odd.length.pfactors) n = -1
     | otherwise = 1

-- sqfree :: N->Bool --a bit faster than sqrfree
-- sqfree n = and[fpsinnum n p==1|p<-pfactors n]
--

totient :: N->N
totient n = sum[1 |k<-[1..n], gcd n k==1]

phi :: N->N --fastest phi so far
phi z = hot (hotmomma z)
 where
  hot [] = 1
  hot ((a:b:cs):ds)= (a^b - (a^(b-1))) * hot ds

totphi :: N->N
totphi m = let dqs = unzip [(p-1,p)|p<-pfactors m] in
  ((* m).product.fst) dqs `div` ((product.snd) dqs)
tot :: N->N
tot m = let f = fromIntegral in
  let g = truncate in
  (g.(* (f m)).product) [ 1 - ( 1/(f p) ) | p<-pfactors m]

multip :: (N->N)->Int->Bool
multip f seed= and[(f a)*(f b)==f(a*b)|(a,b)<-rcoprimes seed]

{--
Take a pair of functions, check
that probably:
f'(n) == sum [f(d) | d<-ffactors n]
for all n, then
compute f(n) 
by sum[mu(d)*f'(n/d)|d<-ffactors n]
--}
mobinv :: Int->(N->N)->(N->N)->N->N
mobinv tol f f' n | and[f' k==sum [f(d) | d<-ffactors k]|k<-boredoms tol] ==False = 0
      | otherwise = invert f' n
  where
  invert f' n = sum [ mu d*f' (n`div`d)|d<-ffactors n] 
{--
note: mobinv doesn't seem very useful.
The role of mobius inversion in number
theory is probably more inline with
proving theorems than simplifying 
calculations. Though I could be wrong.
--}
munder::N->([N],[N])
munder n = (ffactors n,[mu d|d<-ffactors n])
--(\k->[(sum.snd.munder)n|n<-[1..k]])50

--Inclusion-Exclusion
inxs :: [N]->N->N--the subset of [1..n] not divisible by numbers in as.
inxs as n = sum [(n`div`lsd m)*(-1)^(size m)|m<-subsets as]
      where lsd = foldr (lcm) 1 
  --note: no element of as should be a part of any other.
--

{--
factoring in a clean way 
--}
neatfac :: N->[N]
neatfac z = neat z (pfactors z)
 where
  neat 1 d= [1]
  neat n (d:ps) | pano n d ==1 = neat n ps
    | otherwise = (pano n d):neat(n`div`(pano n d)) ps

pano :: N->N->N --pans out all ps from a given z
pano z p = pand z p 0
 where
  pand n d a | gcd n (d^a) == gcd n (d^(a+1)) = d^a
       | otherwise = pand n d (a+1)

---
{--
--}


------helpers
dgnl :: a->(a,a)
dgnl a = (a,a)
rootn :: N->N
rootn n = root n+1
boredoms :: Int -> [N]
boredoms zola = take zola (((map toInteger).randomRs (0,2^30::Int)) (mkBlanket zola))
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
subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = yss++map(x:)yss
  where yss = subsets xs




------------------Elevens Tricks as Blinkylights
type Index = Integer

---------as Lists

bk n = list2N [ blink n (fromInteger i)|i<-[1..((lenN n)+1)]]

blink :: N->Int->N
blink ns i = (beta.(drop (i-1))) (0:(n2List ns)++[0])

beta :: [N]->N
beta (b:[]) = b
beta (a:b:cs) = (a+b+wick(b:cs)) `mod` 10

wick :: [N]-> N
wick (b:[])= b
wick (a:b:cs)   |a+b<9  = 0
    |a+b==9 = wick cs
    |otherwise  = 1
--listly helpers
list2N [] = 0
list2N (n:ns) = n * 10^(length ns) + list2N ns

n2List :: N -> [N]
n2List n = take (fromInteger n) $(n2List (n`div`10))++[n`mod`10]

lenN :: N->N
lenN 0 = 0
lenN n = 1 + (lenN.div n) 10
lminus::Index->N->N
lminus i n= lenN n - i 

onesN :: N->N
onesN 0 = 0
onesN n = 10^(lminus 1 n)+(onesN.div n) 10 

-----------------------Pollard Method
--different poly's (u^2+1) give different results
pollard n = head $ dropWhile (== 1) [ gcd (j-i `mod` n) n | (i, j) <- por n ]
por m = [ ( 2^n `mod` m,  2^(2*n) `mod` m)| n <- [1..m]]

pollardFactors 1 = []
pollardFactors n = pollard n : pollardFactors (n `div` pollard n)


