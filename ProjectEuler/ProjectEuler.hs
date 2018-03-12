{-- This module contains functions
    for finding solutions to the 
   challenges posted @ Project Euler.org
--}

--Solved but not entered: 20 , 35 , 25 , 40 , 32 , 47

module ProjectEuler where
import SortsShuffles
import Data.List 
import Data.Char
import Primes
import Triangles -- for 102

type Lattice = [[Z]]
type R = Float
type Q = Rational

cls :: IO()
cls = putStr "\ESC[2J"

{--
Challenge One:
Find the sum of all the multiples of 3 or 5 below 1000.
--}

divbyNnM :: Z -> Z -> [Z]
divbyNnM n m = [ x | x<-[1..] , (x `mod` n == 0) || (x `mod` m == 0) ] 

challenge1 :: Z
challenge1 = (sum.takeWhile (<1000)) (divbyNnM 3 5)

{--
Challenge Two:
By considering the terms in the Fibonacci sequence
whose values do not exceed four million, find the sum of the even-valued terms.
--}

fibonacci :: [Z] -> [Z] -- This shit is fast 
fibonacci [] = fibonacci [0,1]--Check against popular fib method
fibonacci (x:xs) = x + head xs:fibonacci (x+ head xs: (x:xs))

ffib :: [Z]
ffib = f [0,1] where f (x:xs) = x + head xs:f (x+ head xs: (x:xs))

fibbly :: [Z]
fibbly = 1:1:[(fibbly!!k)+(fibbly!!(k+1))|k<-[0..]] 

fibber :: [Z] -- really very slow
fibber = [ fib k | k<-[0..]]
---------
fib :: Z -> Z -- fib n = nth fibonacci number
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

phimate' :: Z -> Z --phimate' (nth fibonacci number) = n
phimate' n = ((floor.logBase phi) (fromIntegral n)) + 2
---------

challenge2 :: Z
challenge2 = (sum.takeWhile (<4000000)) [ x | x <- (fibonacci [0,1]) , even x]

{--
Challenge Three:
What is the largest prime factor of the number 600851475143 ?
--}
challenge3 :: Z
challenge3 = (head.last.uniquefac) 600851475143

{--
Challenge Four:
Find the largest palindrome made from the product of two 3-digit numbers.
--}

palindrome  :: Z -> Bool
palindrome n = (baseList n 10) == ((reverse.baseList n) 10)

challenge4 :: Z--This can be made much more efficient by careful attention to lists.
challenge4 = maximum [ x*y | x<-[100..999] , y<-[100..999] , palindrome (x*y) ]

{--
What is the smallest positive number that is evenly
divisible by all of the numbers from 1 to 20?
--}

challenge5 :: Z -> Z --challenge5 20
challenge5 n = product [ p^m | p<-(takeWhile (<n) primes'),
                               m<-[1..(n `div` 2)],
                               p^(m+1)  >  n  &&  p^m  <=  n] 
{--
Challenge Six:
What is the difference between the sum of the squares and the square of the sums?
--}
challenge6 :: Z
challenge6 = (sum [1..100])^2 - sum(map (^2) [1..100]) 

{--
Challenge Seven:
Find the 10001st prime.
--}
challenge7 :: Z     
challenge7 =  (last.take 10001)  primes'

{--
Challenge Eight:
Find the greatest product of five consecutive digits in the 1000-digit number.
--}
biggy :: Z
biggy = 731671765313306249192251196744265747423553491949349698352031277450632623957831801698480186947885184385861560789112949495459501737958331952853208805511125406987471585238630507156932909632952274430435576689664895044524452316173185640309871112172238311362229893423380308135336276614282806444486645238749303589072962904915604407723907138105158593079608667017242712188399879790879227492190169972088809377665727333001053367881220235421809751254540594752245258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450
{-- The best I can think to do is to:
breakup the number into sequences with a break at every 0
discard sequences less than 5
grab all subsequences of length 5
1)start multiplying 
or
2) qsort each subsequence
   qsort the collection of all subsequences
   multiply each starting from largest qsorted
I am probably not going to make such a program.
Though it makes me want to learn more about parsing and data structures.
--}

{--
Challenge Nine:
There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.
--}

challenge9 :: Z  
challenge9  = maximum [product x | x<-(unsummedchal9 1000)]

unsummedchal9 :: Z-> [[Z]]
unsummedchal9  = (xyandz.chalist9) 

chalist9 :: Z->[[Z]]
chalist9 n = [x:a:[] | x<-[1..n], a<-(ffactors n) , x*(x+1) == (n`div`a)]

xyandz :: [[Z]] -> [[Z]]
xyandz[] = []
xyandz (z:zs) = let a =  z!!1 in
                let x =  z!!0 in            
                    if  (sq.mkRatio (a*x^2-a)) 2 == 0
                    then xyandz zs
                    else 
                    [ x*a : (a*x^2-a) `div` 2: ((a*x^2+a) `div` 2 ):[]]  ++ xyandz zs

mkRatio ::  Z -> Z -> (Z,Z)
mkRatio n m = if m/= 0 
              then (n `div` (gcd n m) , m `div` (gcd n m) )
              else (0,0)
              
sq :: (Z,Z)->Z
sq (_,0) = 0
sq (0,_) = 0
sq x = if snd x == 1 then fst x else 0


{--Challenge 10:
Find the sum of all the primes below two million.
--}

challenge10:: Z
challenge10 = (sum.takeWhile (<=2000000)) [p | p<-[2..],
  let xs = [1..(floor.sqrt.intToFloat) p] in 
   [ x  | x <- xs, p`mod`x == 0 ] == [1] ]

{--Challenge 12
What is the value of the first triangle number to have over five hundred divisors?

The method:
1. notice many numbers have the same number of factors.
2. Take a large enough sample of numbers to find some with 500 or more factors.
3. 540 happened more often than 504 or anyother so guess this might be more probable.
4. first occurrence of 540 found by random sample to be 32986800.
5. count back to the beginning from this number checking for a number smaller
   with said property.
--}

--76576500 was found with 576 factors. 12375^2+12375 `div` 2.

challenge12 ::  Z --Takes awhile but eventually returns the result.
challenge12  =  head [t | t<-trinum , ((length.ffactors) t > 500)]

challenge12' ::  [(Z,Int)]
challenge12'  =  [(t,(length.ffactors) t) | t<-trinum , ((length.ffactors) t < 600)]

trinum :: [Z]
trinum = [ (n^2 + n) `div` 2 | n<-[0..] ]

nthfromTri :: Z -> Float
nthfromTri t = (-1.0 + sqrt((8.0*fromIntegral t) + 1.0) ) / 2.0

triedntrue :: Z -> Bool  --decides whether a number is a triangle number
triedntrue n = ( floor(nthfromTri n)) == (ceiling(nthfromTri n))

sampling :: Z -> [(Z,Int)] 
sampling n = [ (t,(length.ffactors) t) | t<-(dropWhile (<n)trinum),(length.ffactors) t > 300] 

{--Challenge 13: find the first 10 digits of the summation of 50 onehundred digit numbers--}
challenge13 :: String -- didn't feel like writing this one out.
challenge13 = take 10 "5537376230390876637302048746832985971773659831892672"


{--Challenge 16: sum all of the digits in the number 2^1000--}

challenge16 :: Z
challenge16 = (sum.baseList (2^1000)) 10 --1366

{--Challenge 20: Sum the digits in the number 100!--}

challenge20:: Z --NOTE: not yet entered into ProjectEuler.org
challenge20 = (sum.baseList (fact 100)) 10

{--Challenge 24
What is the 1,000,000th Lexicographic permutation of 0123456789 ?
answer: 2783915460
--}
--gotsta keep n < size([a]!). permcode maybe has the bug?
challenge24 :: (Eq a)=>[a]->Z->[a] --counts from 0th, so 999999th.
challenge24 as 0 = as
challenge24 as n = (reverse.snd.fst.snatch) ((as,[]),(permcode' as n))
   where permcode' as n = (permcode as n)++replicate (length as -(length.permcode as) n) 0

--Returns unique permutation identifier. 
permcode :: [a]->Z->[Z]
permcode (x:xs) 0 = [0]
permcode [] _ = []
permcode (x:xs) n = let  d  =  (size(x:xs))-1 in
              let blim d n = [a | a <-[1..n],(fact d)*a <= n] in
              if blim d n == []
              then 0:(permcode xs n)
              else ((maximum.blim d) n):
                   (permcode xs (n - (fact d *(maximum.blim d) n)))
              where fact n = product [1..n]

snatch :: (Eq a)=> (([a],[a]),[Z]) -> (([a],[a]),[Z])
snatch (([],y),(n:ns)) = (([],y), ns)
snatch (([],y),[]) = (([],y),[])
snatch ((x,y),(n:ns))= snatch ( (filter (/= x!!!n) x, x!!!n:y) , ns )

(!!!) :: [a]->Z->a
(x:xs) !!! 0 = x
(x:xs) !!! n = xs !!! (n-1)

{-- Challenge25: What is the first term in the Fibonacci
            sequence to contain 1000 digits?  --}

challenge25 :: Int --the answer is 4782
challenge25 = head [ snd a | a<-chubbly , fst a == charles ]

charles :: Z
charles = head [o | o <-fibonacci [0,1] , (size.baseList o) 10 >= 1000]

chubbly :: [(Z,Int)]
chubbly = (1,1):(1,2):[ ( fst(chubbly!!k) + fst(chubbly!!(k+1)) , k+3) |k<-[0..]] 

chalice25 :: Z -> Z     --digitlength --> first number of that type.
chalice25 honey = head [o | o <-fibonacci [0,1] , (size.baseList o) 10 >= honey]

challenge26 :: Z
challenge26 = snd.maximum $ [ (cycleP k, k) | k <- takeWhile (< 10^3) eratosthenes2]

eratosthenes2 :: [Z]
eratosthenes2 = 2 : sieveP [1..] 2
  where
    sieveP (n:ns) xs | gg (2 * n + 1) xs = sieveP ns xs
                     | otherwise = (2 * n + 1) : sieveP ns ((2 * n + 1)*xs)
    gg n m = gcd n m > 1

-- counts the length of a decimal cycle, 1/n. thanks internet.
period n = head $ [ p | p <- [1..], (10^p - 1) `mod` n == 0 ]

cycleP :: Z -> Z -- counts the length of a decimal cycle, 1/n.
cycleP n = ff n 1
  where
    ff n k | mod n 2 == 0 = ff (div n 2) k
           | mod n 5 == 0 = ff (div n 5) k
           | mod (10^k) n == mod (10^(k+1)) n = 1
           | mod (10^k) n == 1 = k
           | otherwise = ff n (k + 1)

challenge28 :: Z
challenge28 = foldr ((+).spiral) 0 [2,4,6,8] - 3

spiral :: Integer -> Integer
spiral n = ff n 1 500
  where
    ff n a 0 = 1
    ff n a c = a + n + ff (n + 8) (a + n) (c - 1)


--an exciting thought
phi :: Double
phi = ( (1+sqrt 5)/2)

phimate :: Z -> Z --approximates fibs pretty well: phimate 144 = 12th fib
phimate n = ((floor.logBase phi) (fromIntegral n)) + 2
--making a more accurate for large numbers log, exp, sqrt would be nice.

{--Challenge32:
Find the sum of all products whose multiplicand/multiplier/product
	 identity can be written as a 1 through 9 pandigital.--} 

challenge32:: Z     --The answer is 45228.
challenge32 = let b = raptors in
   (sum.remdups)[  n  | n<-[1000..9999] , j<-[0..length(b n)-1] ,
    qsort(show((b n)!!j!!0)++show((b n)!!j!!1)++show n) == "123456789"] 

remdups :: Eq a => [a]->[a]  --The function I have been wanting all this time!
remdups [] = []               -- This can make lists into sets
remdups (x:xs) = x:remdups (filter (/= x) xs)

raptors ::  Z -> [[Z]] -- fast factoria
raptors n = let xs = [1..(floor.sqrt.intToFloat) n] in 
            qsort'[ [x,n`div`x]  | x <- xs, n`mod`x == 0 ]

burts :: [([Z],Z)]
burts = let b = raptors in
   [ ( (b n)!!j , n ) | n<-[7500..61728394] , j<-[0..length(b n)-1] ,
    qsort(show((b n)!!j!!0)++show((b n)!!j!!1)++show n) == "123456789"] 
--Ali found that the multipliers need not exceed 5 digits together. 

 
--Challenge35: How many circular primes are there below one million?

challenge35 :: Int --answer is 55
challenge35 = length [ p | p<-(takeWhile (<1000000) fprimes) , rotaprime p == True]

rotate :: Int -> [a]->[a]
rotate n xs = drop n xs++take n xs

rotaprime :: Z -> Bool
rotaprime gian = and [ (prime.read.rotate (fromIntegral n)) (show gian) 
          | n<-[0..((size.show) gian) - 1] ]
---
chal35 :: Int
chal35 = length [ p | p<-(takeWhile (<1000000) fprimes), rota' p == True ]

chal35list :: [Z]
chal35list = [ p | p<-(takeWhile (<1000000) fprimes) , rota' p == True ]

rota' :: Z -> Bool
rota' gian = (and.snd.carlo) ((gian,((size.show) gian)-1),[])

carlo :: ((Z,Z),[Bool]) -> ((Z,Z),[Bool])
carlo ((p,0),xs) = ((p,0),xs)
carlo ((p,s),xs) =
    if (and xs) == False
    then ((p,0),[False])
    else carlo ((p,(s-1)),((prime.fromIntegral.read.rotate (fromIntegral s)) (show p)):xs)

{--Challenge36: Find the sum of all numbers less than one million,
	which are palindromic both in base 10 and 2 --}
challenge36 :: Z
challenge36 = sum tenthentwo

tenthentwo :: [Z]
tenthentwo = [ a | a<-[1..999999] , palindrome a , (palindrome.inBase 2) a ]

inBase :: Z->Z->Z
inBase b d | div d b == 0 = mod d b
           | otherwise = mod d b + 10 * inBase b (div d b)

{--Challenge40:  An irrational decimal fraction is created by 
		concatenating the positive integers:
		0.12345678910-1-112131415161718192021...
--}
challenge40 :: Z --The Answer is 3125
challenge40 = lebron 6
  where
    lebron 0 = bogus $ irradecimal!!1
    lebron n = bogus(irradecimal!!(10^n)) * (lebron (n-1))
    bogus dude = fromIntegral (ord dude - 48)     

rassilon :: String
rassilon = "0123456789101112131415161718192021"

irradecimal :: String
irradecimal = "0123456789"++ (spunion 1) 
  where
    spunion c = ((head.show) c):intersperse ((head.show) c) "0123456789" ++ spunion (c+1)
              

{--Challenge47: Find the first four consecutive integers 
		to have four distinct prime factors.
	 	What is the first of these numbers?--}

challenge47 :: Z -- The answer is 134043
challenge47 = head [j | j<-[0..] , and [(length.pfactors) k == 4 | k<-[j..j+3]] ]

chal47' :: (Z,Z,Z,Z)
chal47' = head [(j,j+1,j+2,j+3) | j<-[0..] , and [(length.pfactors) k == 4 | k<-[j..j+3]] ]

challenge102 :: Int
challenge102 = length.filter allQuads $ triangles

data Quadrant = I | II | III | IV deriving (Eq, Show)
data Segment = S Point Point deriving (Eq, Show)
type Triangle = [(Double, Double)]
type Point = (Double, Double)

edges :: Triangle -> [Segment]
edges [a, b, c] = [S a b, S a c, S b c]

endToQuad :: Triangle -> [Quadrant]
endToQuad [] = []
endToQuad (pt:pts) = f pt : endToQuad pts
  where
    f (x,y) | and [x >= 0, y >= 0] = I
    f (x,y) | and [x <= 0, y >= 0] = II
    f (x,y) | and [x <= 0, y <= 0] = III
    f (x,y) | otherwise = IV

ceptToQuad :: Point -> [Quadrant]
ceptToQuad (0,0) = [I, II, III, IV]
ceptToQuad (0,y) | y > 0 = [I, II]
                 | otherwise = [III, IV]
ceptToQuad (x,0) | x > 0 = [I, IV]
                 | otherwise = [II, III]

xycept :: [Segment -> Point]
xycept = [xcept, ycept]
  where
    ycept (S (a,b) (c,d)) = (0 ,(a*d - c*b) / (a-c))
    xcept (S (a,b) (c,d)) = ((c*b - a*d) / (b-d), 0)

lineMember :: Segment -> Point -> Bool
lineMember (S (a,b) (c,d)) (p,q) | a < c = f a c p
                                 | otherwise = f c a p
  where
    f x y t = and [x<t, t<y] 

allQuads :: Triangle -> Bool -- in all Quads
allQuads tri = (length.ceptData) tri == 4

ceptData :: Triangle -> [Quadrant]
ceptData tr = cleanConcat $ endToQuad tr : (map quadCepts (edges tr))
  where
    cleanConcat = remdups.concat
    remdups [] = []
    remdups (x:xs) = x : remdups [t | t<-xs, t /= x]

quadCepts :: Segment -> [Quadrant]
quadCepts seg = f $ xycept <*> [seg]
  where
    f = concat.(map ceptToQuad).filter (lineMember seg)

{--Challenge544
hotmomma 1112131415
[[5,1],[17,1],[59,1],[211,1],[1051,1]]

Let F(r,c,n) be the number of ways to color a rectangular grid with r rows and c columns using at most n colors such that no two adjacent cells share the same color. Cells that are diagonal to each other are not considered adjacent.

For example, F(2,2,3) = 18, F(2,2,20) = 130340, and F(3,4,6) = 102923670.

Let S(r,c,n) = ∑nk=1∑k=1n F(r,c,k).

For example, S(4,4,15) mod 109+7 = 325951319.

Find S(9,10,1112131415) mod 109+7.
--}

--challenge544 :: Z

{--Challenge616
Alice plays the following game, she starts with a list of integers L and on each step she can either:

* remove two elements a and b from L and add a^b to L
* remove an element c from L that can be written as a^b, with a and b
  being two integers such that a, b > 1, and add both a and b to L

For example starting from the list L={8}, Alice can remove 8 and add 2 and 3
resulting in L={2,3} in a first step. Then she can obtain L={9} in a second step.

Note that the same integer is allowed to appear multiple times in the list.

An integer n>1 is said to be creative if for any integer m>1 Alice can obtain
a list that contains m starting from L={n}.

Find the sum of all creative integers less than or equal to 10^12.


NOTES:
n must start with a number expressible as a power.
--}
