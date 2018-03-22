module Primes where
-- import Math.NumberTheory.Primes.Factorisation -- not used, but useful
-- import Data.Numbers.Primes  -- not used, but useful
import System.Random
import Printables
import SortsShuffles
import Typehandling
import Data.List

type Z = Integer
type Prime = Integer
type Base = Z
type Rand = Z



-- :set +s for testing run time speed
-- Cleaner is a function to work on that 
-- would make probable prime checking much faster. . .


phone :: Int -> Int --Try phone with area code here.
phone n = n

alis :: Z
alis = 82377

home :: Z
home = 5125241285

hotmomma :: Z -> [[Z]]
hotmomma n = uniquefac n

blue :: Z
blue = 2^23 --or 8388608


{--To do: 
Chinese Zodiac computer
3191981 
--}
---------factorization functions and Prime Spitters

-- infinite case
primes' :: [Prime]     -- take 10 primes' = [2,3,5,7,11,13,17,19,23,29]
primes' = sieve [2..]  --takeWhile (<10) primes' = [2,3,5,7]

sieve :: [Z] -> [Prime]
sieve (p:xs) = p:sieve [x | x<-xs, mod x p /= 0]

         --verifies that factors of p upto sqrt of p are exactly [1].
fprimes :: [Prime]
fprimes = [p | p<-[2..],
  let xs = [1..(floor.sqrt.intToFloat) p] in 
   [ x  | x <- xs, p`mod`x == 0 ] == [1] ]

fprimes' :: [Prime] --doesn't count even primes but is it really faster?
fprimes' = 2:[p | p<- [1..], odd p ,--doesn't seem so.
  let xs = [1..(floor.sqrt.intToFloat) p] in 
   [ x  | x <- xs, p`mod`x == 0 ] == [1] ]
-- ################ work in progress
    --- psuedoprimes
 -- mkBlanket :: Int -> StdGen
 -- mkBlanket cozy = mkStdGen cozy

-- These aren't as good as pPrime as found in Pprimes.hs anyway
lil'toller :: Prime->Int-> Bool  -- why is this so much faster when clean is so much slower?
lil'toller red int = (and.take int) troofs == True
            where troofs = [ (a^red) `mod` red == a | a <-(map toInteger (clean red)) ]

lil'tollerz :: Prime->Int-> Bool  
lil'tollerz red int = ((and.take int) $ [ (a^red) `mod` red == a | a <-(knuffle [2..(red`div`2)]) ]) == True
-- knuffle may only need to go [2..(red `div`2)]
 
clean :: Integral a => a -> [Int]
clean thong = let icky = fromIntegral thong - 1 in
 take icky ((lookout.randomRs (1,icky)) (mkBlanket 42) )
 where
 lookout [] = []
 lookout (phaker:xs) = phaker:lookout [x |x<-xs, x/=phaker]
 -- suggest an n and receive a random permutation of the list [1..n]

-- Using pmonicrandos bs I want to make a better shuffle
-- knuffle from SortsShuffles might do it
-- ##############

laban :: Int -> [Prime]
laban smit = [ p | p<-[2..], lil'toller p smit]


-- IN answer to that below: consider BirthdayProb for generating Perms!!!
{--Can the prob. method be made more efficient by using as an alternative to clean,
a function which gives (random number,new seed) and utilizes if/then style arguments.
Perhaps something like,
If True that p returns prime then prime, 
Otherwise try for new seeded random number.
If number of randoms tried is more than half,
return p is not prime.

Still intuitively, this seems to require at least the same number of steps.
Lot's of computation seems inevitable for screening out previously used 
random values.

--}
     --------------
--finite case 
primes :: Z -> [Z]
primes n = [p | p<-[2..n], ffactors p == [1,p]]

primestoP :: Prime -> [Z]
primestoP p = takeWhile (<= p) fprimes

--Prima Factoria
prime :: Prime -> Bool
prime p = ffactors p == [1,p]

ffactors ::  Z -> [Z] -- fast factoria
ffactors n = let xs = [1..(floor.sqrt.intToFloat) n] in 
              qsort'( [ x  | x <- xs, n`mod`x == 0 ]
         ++reverse[n`div`x | x <- xs, n`mod`x == 0 ])

qsort' :: Ord a => [a] -> [a]  -- removes duplicates. Why?
qsort' [] = []
qsort' (x:xs) = qsort' smaller ++ [x] ++ qsort' larger
           where
             smaller = [s | s<-xs, s<x]
             larger  = [l | l<-xs, l > x]

pfactors :: Z -> [Prime] --returns list of prime factors to be found in a number n.
pfactors n = [p | p<- ffactors n, prime p] 

fpsinnum :: Z->Prime->Z
fpsinnum 0 p = 0
fpsinnum n 0 = 0
fpsinnum n p = sum(if b == True
                then 1:[fpsinnum (n`div`p) p]
                else  [])
              where b = n`mod`p == 0

uniquefac :: Z -> [[Z]] --12456 -> [[2,3],[3,2],[173,1]]
uniquefac n = [p:(fpsinnum n p):[]|p<-pfactors n]

uniqfact  :: Z->[[Z]] --12456 -> [[2,2,2],[3,3],[173]]
uniqfact n = [map (*(head p))(ones(last p))|p<-uniquefac n] 


-----Number Theory overflow------

fpTTat1 :: Z->Z --number of factors in a number
fpTTat1 n = size(ffactors n) 

pTTat1 :: Z->Z --as you will find it in Niven
pTTat1 n = product[(fpsinnum n p)+1|p<-(ffactors n), prime p]

factors :: Z -> [Z] --slow but clear
factors n = [x | x <- [1..n], n `mod` x == 0]

ffactors' :: Z -> [[Z]] --Useful in this form for Ali's factoring method
ffactors' n = qsort [ x:(n`div`x):[] |
                     x <- [1..(floor.sqrt.intToFloat) n]
                            , n`mod`x == 0 ]
trinum' :: [Z]
trinum' = scanl (+) 0 [1..]


    --Try to polymorphize up2lesseq with primeunder
     --maximum multiple of f(d) not exceeding n
up2lesseq :: (Z->Z)->Z->Z->Z
up2lesseq f d n = maximum[x |x<-[1..n] , (f d)*x <= n]
     --think a  s.t  p^a||n
primeunder :: (Z->Z->Z)->Z->Z->Z
primeunder f p n = maximum[a |a<-[1..n] , f n (p^a) <= 0]

factorial :: Z -> Z
factorial 0 = 1
factorial n = n*factorial (n-1)

fact :: Z->Z
fact n = product [1..n]

--method Ali and I used at Kerby Lane for deciding whether a number is prime
intToFloat :: Integer -> Float
intToFloat n = fromInteger (toInteger n)

root ::Z -> Z
root  = (floor.sqrt.intToFloat)

        --45 `willdivide` 5
willdivide :: Z ->Z -> Bool
willdivide n d = mod ((sum.baseList n) (d+1)) d == 0 

baseList :: Z -> Base -> [Z] --baseList 4 2 = [0,0,1]
baseList 0 _ = []
baseList _ 0 = []
baseList n 1 = ones n
baseList n b = mod n b:(baseList.div n) b b 

ones :: Z -> [Z]
ones 0 = []
ones n = 1:ones (n-1)

-- qsort :: Ord a => [a]->[a]  --This even works over sets of sets!
-- qsort [] = []
-- qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
            --where
              --smaller = [s | s<-xs, s<=x]
              --larger  = [l | l<-xs, l > x]
        
taKe :: Z -> [a] -> [a]
taKe 0 _ = []
taKe _ [] = []
taKe n (x:xs) = x: taKe (n-1) xs

size :: [a]->Z  --size [[1,1]] = 1
size = foldr oneplus 0
       where oneplus x n=n+1

divmod :: (Integral a) => a -> a -> [a]
divmod n d = (n `div` d) : (n`mod`d):[]

-----tests for random
watermelon_man :: Prime -> [(Bool,Z)]  
watermelon_man reggae = [ ((a^reggae) `mod` reggae == a , a ) |
      a <-(map toInteger (clean reggae)) ]
troot15 :: [Bool]
troot15 = let red = 15 in [ (a^red) `mod` red == a | a <-(map toInteger (clean red)) ] 

------------------- Density of Factors

-- density helpers
run = [1..]
q2f :: (Z,Z)->Float
q2f (a,b) = (fromInteger a)/(fromInteger b)
ratio = (q2f.re)

hot2trot n = [(head x,(head.tail)x)|x<-hotmomma n]

re :: (Z,Z) -> (Z,Z) --reduces fractions.
re (a,b) = (a `div` (gcd a b), b `div` (gcd a b))

sqfree :: Z->Bool --a bit faster than sqrfree
sqfree n = and[fpsinnum n p==1|p<-pfactors n]
------

totient n = product [p^l - p^(l-1) | (p,l)<-hot2trot n]
nonpTTat1 n = n - pTTat1 n
nonrelTT n = n - pTTat1 n - totient n + 1


--deal 20 (nonpTTat1) (totient)
deal n f g = (n,toss n (0,0,0) f g (1,1))
   where 
     toss 0 ks _ _ (i,j) = (ks,(i,j)) 
     toss n (i,j,k) f g (a,b)| (f n) < (g n) = toss (n-1) (i+1,j,k) f g (re (i+1,k))
                             | (f n) == (g n) = toss (n-1) (i,j+1,k) f g (i,k)
                             | (f n) > (g n)  = toss (n-1) (i,j,k+1) f g (re (i,k+1))

teal ls f g = (toss ls (0,0,0) f g (1,1))
   where 
      toss [] ks _ _ (i,j) =  (ks,(i,j)) 
      toss (x:xs) (i,j,k) f g (a,b)| (f x) < (g x) = toss xs (i+1,j,k) f g (re (i+1,k))
                                   | (f x) == (g x) = toss xs (i,j+1,k) f g (i,k)
                                   | (f x) > (g x)  = toss xs (i,j,k+1) f g (re (i,k+1))


listedDensity = -- ( n , (<,==,>), ratio (nonrelTT/totient))
         do cls;
            showcells 1 $ take 50 [deal i (nonrelTT) (totient)|i<-[(600+1)..]]; 
            showcells 50 $ take 50 [deal i (nonrelTT) (totient)|i<-[(600+51)..]]; 
            showcells 100 $ take 50 [deal i (nonrelTT) (totient)|i<-[(600+101)..]];
            showcells 150 $ take 50 [deal i (nonrelTT) (totient)|i<-[(600+151)..]]; 
            showcells 200 $ take 50 [deal i (nonrelTT) (totient)|i<-[(600+201)..]]; 
            showcells 250 $ take 50 [deal i (nonrelTT) (totient)|i<-[(600+251)..]]  

blistedDensity = -- ( n , reduced ratio of nonrelTT to totient)
         do cls;
            showcells 1 $ take 50 [(i,re (nonrelTT i, totient i))|i<-[(1+1)..]]; 
            showcells 20 $ take 50 [(i,re (nonrelTT i, totient i))|i<-[(1+51)..]]; 
            showcells 40 $ take 50 [(i,re (nonrelTT i, totient i))|i<-[(1+101)..]];
            showcells 60 $ take 50 [(i,re (nonrelTT i, totient i))|i<-[(1+151)..]]; 
            showcells 80 $ take 50 [(i,re (nonrelTT i, totient i))|i<-[(1+201)..]]; 
            showcells 100 $ take 50 [(i,re (nonrelTT i, totient i))|i<-[(1+251)..]];
            showcells 120 $ take 50 [(i,re (nonrelTT i, totient i))|i<-[(1+301)..]]; 
            showcells 140 $ take 50 [(i,re (nonrelTT i, totient i))|i<-[(1+351)..]]; 
            showcells 160 $ take 50 [(i,re (nonrelTT i, totient i))|i<-[(1+501)..]];
            showcells 180 $ take 50 [(i,re (nonrelTT i, totient i))|i<-[(1+551)..]];
            showcells 200 $ take 50 [(i,re (nonrelTT i, totient i))|i<-[(1+551)..]]; 
            showcells 220 $ take 50 [(i,re (nonrelTT i, totient i))|i<-[(1+601)..]]; 
            showcells 240 $ take 50 [(i,re (nonrelTT i, totient i))|i<-[(1+651)..]];
            showcells 260 $ take 50 [(i,re (nonrelTT i, totient i))|i<-[(1+701)..]]; 
            showcells 280 $ take 50 [(i,re (nonrelTT i, totient i))|i<-[(1+751)..]]; 
            showcells 300 $ take 50 [(i,re (nonrelTT i, totient i))|i<-[(1+801)..]];
            showcells 320 $ take 50 [(i,re (nonrelTT i, totient i))|i<-[(1+851)..]]; 

listedtot2nonpTT n =  [beal i nonrelTT totient |i<-[n..]] --not keeping stats

beal n f g = (n,toss n (0,0) f g 0)
   where 
     toss 0 ks _ _ rs = rs
     toss n (i,k) f g rs | (f n) < (g n) = toss (n-1) ((i+1,k)) f g (fromInteger (i+1) / fromInteger k) 
                         | (f n) > (g n)  = toss (n-1) ((i,k+1)) f g (fromInteger i / fromInteger (k+1)) 
                         | otherwise = toss (n-1) (i,k) f g (fromInteger i / fromInteger k)



