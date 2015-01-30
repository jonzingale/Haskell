module Hutton where
import Data.Char
type Base = Integer
type Lattice = [[Z]]
type Prime = Z
type Z = Integer

cls :: IO()
cls = putStr "\ESC[2J"


-------That all three of these are combinatorial is neat.
--Powerset
subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = yss++map(x:)yss
	where yss = subsets xs

--puts an x in each position of a list
interleave :: a->[a]->[[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys):map (y:) (interleave x ys)

--all permutations of a given list or string
perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map(interleave x) (perms xs))
--------------

partition' :: (a -> Bool) -> [a] -> ([a],[a])
partition' p xs = (filter p xs,filter (not.p) xs)

partition :: (a -> Bool) -> [a] -> [[a]]
partition p xs = [filter p xs] ++ [filter (not.p) xs]

remdups :: Eq a => [a]->[a]  --The function I have wanting all this time!
remdups [] = []               -- This can make lists into sets
remdups (x:xs) = x:remdups (filter (/= x) xs)

qsort :: Ord a => [a]->[a]  --This even works over sets of sets!
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
           where
             smaller = [s | s<-xs, s<=x]
             larger  = [l | l<-xs, l > x]

qsort' :: Ord a => [a] -> [a]  -- removes duplicates. Why?
qsort' [] = []
qsort' (x:xs) = qsort' smaller ++ [x] ++ qsort' larger
           where
             smaller = [s | s<-xs, s<x]
             larger  = [l | l<-xs, l > x]

-- parts :: Z -> [Z] -> [Lattice] --all the ways to partition [Z]  into Z parts
-- parts 0 [] = [[]]              --A jump towards answer to the Parentheses problem!!!-spivak
-- parts 0 (x:xs) = []            --The length of each list in the list gives Pascals Triangle!
-- parts (n+1) [] = []
-- parts (n+1) (x:xs) = map (new x) (parts n xs) ++ map (glue x) (parts (n+1) xs)

new :: a -> [[a]] -> [[a]]
new x yss = [x]:yss

glue :: a->[[a]]->[[a]]
glue x (ys:yss) = (x:ys) : yss

countem :: [[Z]] -> Z -> [[Z]] --Selects which elements listSum to a given Integer.
countem xs n = [ l |  l <-xs, listSum l == n ]

ones :: Z -> [Z]     --ones 4 = [1,1,1,1]
ones 0 = []
ones n = 1 : (ones (n-1)) 

baseList :: Z -> Base -> [Z] --baseList 4 2 = [0,0,1]
baseList 0 _ = []
baseList _ 0 = []
baseList n 1 = ones n
baseList n b = mod n b:(baseList.div n) b b 

binar :: Z -> [Z] --binar 6 = [0,1,1]
binar 0 = []
binar n = (mod n 2):binar(div n 2)

bin2int :: [Z] -> Z   -- bin2int [1,1,0] -> 3
bin2int = foldr (\x y->x+2*y) 0


listSum :: [Z] -> Z
listSum [] = 0
listSum (x:xs) = x + listSum xs

comb :: Z -> Z -> Z
comb n k = div (fact n) (fact k * fact (n-k))

fact :: (Integral z) =>z -> z
fact 0 = 1
fact n = n * fact (n-1) 

--Primes
primesto :: Z -> [Prime]
primesto n = [p | p<-[2..n], prime p]

factors' :: Z -> [Z]
factors' n = [x | x <- [1..n], mod n x == 0]

prime :: Prime -> Bool
prime p = factors' p == [1,p]
--Notice that factors' and primesto had to be named funny because of multiple declarations.
--------


--By freeing the generation of prime numbers from the constraint of finiteness,
--we have obtained a modular program on which different control parts can be used
--in different situations. For example.
primes :: [Z]         -- take 10 primes = [2,3,5,7,11,13,17,19,23,29]
primes = sieve [2..]  --takeWhile (<10) primes = [2,3,5,7]

sieve :: [Z] -> [Z]
sieve (p:xs) = p:sieve [x | x<-xs, mod x p /= 0]

--Freqency tables for a give language, Ceasar ciphers, and Chi Squared to match

encode :: Int->String->String
encode n xs = [shift n x | x<- xs]

shift :: Int -> Char -> Char
shift n c | isLower c = int2let (mod (let2int c+n) 26)
          | otherwise = c

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i|(x',i)<-zip xs [0..n], x==x']
                  where n = length xs -1

engFreqTable :: [Float]
engFreqTable = [8.2,1.5,2.8,4.3,12.7,2.2,2.0,6.1,7.0,0.2,0.8,4.0,2.4,
                6.7,7.5,1.9,0.1,6.0,6.3,9.1,2.8,1.0,2.4,0.2,2.0,0.1]

percent :: Int->Int->Float
percent n m = ( toEnum n/ toEnum m) * 100

freqs :: String -> [Float]
freqs xs = [percent(count x xs) n | x<- ['a'..'z']]
            where n = lowers xs

count :: Char -> String -> Int
count x xs = length [x' | x'<-xs, x == x']

lowers :: String -> Int
lowers xs = length [x| x<-xs, isLower x]

---
chisqr :: [Float] ->[Float]->Float
chisqr os es = sum[((o-e)^2)/e|(o,e)<-zip os es]

perfects :: Z -> [Z]
perfects n = [ ns | ns<-[1..n] , listSum(factors ns) == ns]

----Vector Operations
scalarproduct :: [Z]->[Z]->Z
scalarproduct xs ys = sum[ x*y | (x,y)<- zip xs ys]

vectorsum :: [Z]->[Z]->[Z]
vectorsum xs ys = [ x+y | (x,y)<-zip xs ys]


---

factors :: Z -> [Z]
factors n = [ f | f<-[1..(n-1)] ,mod n f == 0]

rotate :: Int -> [a]->[a]
rotate n xs = drop n xs++take n xs

crack :: String -> String
crack xs = encode (-factor ) xs
     where
        factor = head(positions(minimum chitab)chitab)
        chitab = [chisqr (rotate n table')engFreqTable| n<-[0..25]]
        table' = freqs xs


---------------Some Lambda Expressions--------

add :: Z -> Z -> Z
add  = \x -> (\y -> x+y)

cont :: a -> b -> a
cont x = \_ -> x

odds :: Z -> [Z]
odds n = map(\x->x*2+1) [0..n-1]


------------------fixed-point free isos: slow and combinatorial
type Prob=Float
type Hats=Int
type Returnings=[Int]
type Perms=Int
type Wrong=Int

walk n = [0..(n-1)]

fixedfrees :: Hats -> [Returnings]
fixedfrees n = [ k | k<-(perms.walk) n, (not.or.brrr.reverse) k ]
 where 
  brrr [] = []
  brrr (z:zs) = 
    let n = length (z:zs) - 1 in
    (z == n): brrr zs

--number of FP-free Isos
fixedfreesN :: Hats -> Returnings
fixedfreesN w = [(length.fixedfrees) k | k <- walk w]

-- ( hat returnings, wrong hat returnings, chance of being wrong)
returnings :: Hats->[(Perms,Wrong,Prob)]
returnings n = [(ns,k,(fromIntegral ns)/(fromIntegral k))| 
      (ns,k) <- zip (map fact (walk n)) (fixedfreesN n),k/=0]

invreturnings n = [ (a,b,1/c) |(a,b,c)<-returnings n] 












-------------------

