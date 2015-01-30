module Probability where
import Primes
import Approximations

type N = Integer
type Domain = Integer
type Codomain = Integer
type Prob = (N,N)
type Unary = N->N
type Percent = Integer

cls :: IO()
cls = putStr "\ESC[2J"


{--
eta: unital taking an N and returning (N,1)
x : multiplies two fractions of the form (N,N)
reduce: reduces a fraction in (N,N) form
percent :: (N,N)->Float
neg: returns the complementary probability. neg (tarot) = birthday
sumQ: Sums a list of (N,N)-styled fractions
sumq: Sums a pair of (N,N)-styled fractions (w/ reduction)
sump: Sums a pair of (N,N)-styled fractions (w/ lcm)
subq: Subtracts a pair of (N,N)-styled fractions (w/ reduction)

fakt: factorial
choose: n choose k
perms: number of ways to permute k out of n things (n choose k order matters)

monics: probability that an arbitrary map of finite sets D->C is monic
epics: probability that an arbitrary map of finite sets D->C is epic
retractions:prob that an arbitrary map of finite sets C->D is a retraction for a map D->C.
sections:a guess at the prob that an arbitrary map is a section for . . .

headsoutof: probability that m heads in n tosses
loadedheads: probability that m heads in n tosses of a loaded coin

Hypergeometric Distribution (rCk)(bCn-k)/(r+bCn)
alismethod: 22 Maj 56 Min 10 card pull 2 of them Major. . . what are the chances?
redblack: 4 kings 48 non-kings 5 card hand 2 of them kings...what are the chances?
redblackdist: give distribution for various values k (of them kings,above) 
colorballs: a generalization of blkballschoosen .. colorballs [22,2] [56,8] 

--an experiment
last9s :: Unary->[N]->number of N's ending in 9
diggit takes a unary, applies it to primes and askes what some probabilities are
pronine automates diggit
--

nin2ten: replaces the 9's with 0's in a sequence of 1's and 9's
mkprandom: psuedorandom number generator based on primes
prandomlist: a list of psuedorandoms based on mkprandom
--Normalcy
density: gives the normal density for a number of trials and a probability
	  bell for balanced coins, skewed for loaded
distr: gives the normal distribution for a number of trials and a probability
dense: inverse to distr (in a sense)

--
expect: Gives the expectation given [N] and an associated [Prob]
var: Gives the variance given [N] and and an associated [Prob]
standevi: Gives the standard deviation. 
--
probofcube: prob of finding in an (N,a|b) lattice a k dimensional cube. 

--}
eta :: N->(N,N)
eta n = (n,1)
x :: (Integral a) => (a,a)->(a,a)->(a,a)
x (p,q) (r,s) = (p*r,q*s) 
reduce :: (Z,Z) -> (Z,Z) --reduces a fraction.
reduce (a,b) = (a `div` (gcd a b), b `div` (gcd a b))
percent :: (Integral a)=>(a,a)->Percent
percent (n,d) = round (fromIntegral (n*100) / fromIntegral d)
neg :: Prob->Prob
neg (o,t) = (t-o,t)
sumQ :: [(N,N)]->(N,N) --sums a list of (N,N) fractions
sumQ ps= foldr sumq (0,1) ps
sumq :: (N,N)->(N,N)->(N,N) --sums a pair of (N,N) fractions
sumq (p,q) (r,s) = re( p*s+r*q,q*s)
sump (p,q) (r,s) = let d=lcm q s in
 	  (d`div`q*p+(d`div`s*r),d)
subq :: (N,N)->(N,N)->(N,N)
subq ns (p,q) = sumq ns (-p,q)

fakt :: (Integral a)=> a->a
fakt n  | n >= 0 = foldr (*) 1 [1..n]
	| otherwise = 0
choose :: (Integral a)=>a->a->a
choose n k = fakt n `div` (fakt k * fakt (n-k))
perms :: (Integral a)=>a->a->a
perms n k = n`choose`k * fakt k

monics :: Domain->Codomain->Prob
monics d c = (fakt c, fakt (c-d) * c^d)
epics :: Domain->Codomain->Prob
epics d c = (sum [ (-1)^k * (c`choose`k) * (c-k)^d | k<-[0..c]] , c^d)
retractions::Domain->Codomain->Prob --the same prob for all codomains!!
retractions d c= re(d^(c-d),d^c)
{--
sections would be cool but way more challenging. Whould a similar property hold?
I imagine the function should go a little
like this. . .
--}
sections :: Domain->Codomain->Prob
sections d c= re(d`choose`c *fakt c,d^c)

headsoutof :: N->N->Prob
headsoutof m n = (n `choose` m, 2^m*2^(n-m))
loadedheads :: N->Prob->N->Prob
loadedheads m p n =
	(n `choose` m,1) `x` (freek m p) `x` (freek (n-m) (neg p))

alismethod :: N->N->N->N->Prob
alismethod r b n k=
	(n`choose`k,1) `x` ((r`perms`k)*(b`perms`(n-k)),(r+b)`perms`n )
redblack :: N->N->N->N->Prob --4 kings 48 nonkings 5 card hand chances of 2 kings
redblack r b n k = 
	re ( r`choose`k * (b `choose`(n-k)), (r+b)`choose`n )
redblackdistr :: N->N->N->Distribution --trumps? redblackdist 22 56 10 
redblackdistr r b n = let bawl = redblack r b n in
	[bawl k |k<-[1..n]]
colorballs :: [N]->[N]->Prob
colorballs cs ks = let chew = (\y->fst y`choose`snd y) in
	( (product.(map chew)) (zip cs ks) , sum cs `choose` sum ks )



----An experiment
last9s :: Unary->[Prime]->N
last9s f ps = size [nines|nines<-ps,(f nines)`mod`10==9]

diggit :: Unary->[(N,Prime,Percent,Percent)]
diggit z=
 [let nines =((last9s z).(take (fromIntegral n)))  fprimes in
  let sofars= percent(nines,n) in
  let halfies = (percent.neg)(nines`headsoutof`n) in
   (n,p, sofars ,halfies)  |(n,p)<-zip [1..] fprimes]

pronine ::Unary-> IO String
pronine z= prot 1 z
 where prot i z=
	      do
	  	 (putStr.show.head.(drop i)) (diggit z)
		 putChar '\n'
		 if 0 == (i`mod`30)
		  then 
		      do putChar '\n'
			 (putStr.show) i
			 putChar '\n'
			 getLine 
			 prot (i+1) z
		  else prot (i+1) z
----
----an Attempt at psuedorandomness..a very bad one at that.
nin2ten :: [N]->N
nin2ten [] = 0
nin2ten (x:xs)= nin2bin x*2^size xs+(nin2ten xs)
 where 
  nin2bin n|n==1=1
	   |otherwise=0

mkprandom ::N->(N,N)
mkprandom n = let chunk= [ps^2`mod`10|ps<-take 32(drop (fromIntegral n) fprimes)] in
 ((nin2ten.(take 16)) chunk, (nin2ten.(drop 16)) chunk)

prandomlist ::N->[N] --gets on a cycle very quickly
prandomlist n = let (p,s)= mkprandom n in
		 p:prandomlist (s`mod`1993)

{--
Notice: alismethod and redblack are isomorphic up to extension.
`x` is useful for keeping the choosey part separate from the probable parts.
halfies in diggit drops off fast. . how does this relate to repeated coin tosses?
test prandomlist with chisquare
--}

type Trials = N
type Distribution=[Prob]
density :: Trials->Prob->Distribution
density n p=
 [ (n`choose`k,1)`x`
   (freek k p) `x`
   (freek (n-k) (neg p))
   | k<-[0..n]        ]
distr :: Trials->Prob->Distribution
distr n p= let (i:t) = scanl sumq (0,1) (density n p) in t
dense :: Trials->Prob->Distribution
dense n p = doit ((0,1):distr n p)
 where
  doit [b] = []
  doit (a:b:zs) = subq b a:doit (b:zs) 

falling:: N->N->N
falling = (\a n->a^2-2*a*n)
dx :: (Num a,Enum a)=>(a->a)->a->[a]
dx f n= [f s|s<-[0..n]]
nSdt :: (Num a,Enum a)=>(a->a)->a->[a]
nSdt f n= scanl (+) 0 (dx f n)

--------
expect :: [(N,N)]->[Prob]->(N,N)
expect ns ps = sumQ[ n `x` p|(n,p)<-zip ns ps]

--E((X-EX)^2)
var :: [(N,N)]->[Prob]->(N,N)
var ns ps= 
	let xminusEx =  map (subq (expect ns ps)) ns in
	let sqrX = map (freek 2) xminusEx in
	(re.expect sqrX) [(1,1)|i<-sqrX]
 
--sqrt var
standevi :: (Floating float)=>[(N,N)]->[Prob]->(Float,float)
standevi n p = let q= (fromIntegral.fst) in
	       let r= (fromIntegral.snd) in
 	       ((sqrt.q.var n) p,(sqrt.r.var n) p)
---------
probofcube :: N->N->(N,N) 
probofcube n k = (distinctp n`choose`k ,fpTTat1 n)
 where distinctp n=sum[1|f<-ffactors n,prime f]

