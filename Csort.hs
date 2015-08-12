--module Csort {--(cls,cSort,samplesort,cPrimesieve1,cPrimesieve2,
--		randput,naturalV,randV,antarticaV,sampleshuffle,
--		comonadicshuffle,randomV,wordV,navierStokes,blocksV)--} where

module Csort where

import System.Random
import SortsShuffles
import Primes
import Data.Char

type N = Integer
cls = putStr "\ESC[2J"

data U x = U x x
data V x = V [x] [x] [x] --shifting focus left or right
righty (V xs (a:b:[]) (c:d:es)) = V (b:a:xs) [c,d] es --whole shift
lefty (V (b:a:cs) [d,e] fs) = V cs [a,b] (d:e:fs)

left (V (a:as) [b,c] ds) = V as [a,b] (c:ds)--half shift
right  (V as [b,c]  (d:ds)) = V (b:as) [c,d] ds

--V x objects
zahlenV :: (Num a,Enum a)=> V a
zahlenV = V (neg [1..]) [0,1] ([2..70]++[-5]++[71..])
naturalV = V zeros [0,1] [2..]
randV r= V zeros [1,0] (randomRs (0,999) (mkBlanket r))
antarticaV= V zeros [0,1] (clean 1000 ++ [9999..])
rand2V r= V (randomRs (-99,99) (mkBlanket r)) [1,0] (randomRs (-99,99) (mkBlanket r))
--randomV = V ((randoms.mkBlanket)42) [0,1] ((randoms.mkBlanket)41)
wordV =
  V (randomRs ('a','z') (mkBlanket 42)) ['Q','R'] (randomRs ('a','z') (mkBlanket 24))
ringwordV = V it ['a','a'] (['a'|j<-[0..300]]++it)  
 where it = foldr (++) [] $ repeat ([' ']++['a'..'z'])
streakV n =
   V (foldr (++) []  (repeat (['*']++(take n (repeat ' ') )) ))
       ['L','R'] 
         ( foldr (++) [] $ repeat (['*']++(take n (repeat ' ') ))) 
blocksV n =  let it = foldr (++) [] [((take n).repeat) a|a<-['a'..'z']] in
	 V ("FUCK"++it) ['A','Z'] $" FUCK IT INDEED ! ! !"++[' '|j<-[1..(4*n)]]++it

--showing
instance Show a => Show (V a) where
 show (V a b c) = show [a,b,c]
---------

instance Functor V where
  fmap f (V a b c) = V (map f a) (map f b) (map f c)

class Functor w => Comonad w where
  (=>>)    :: w a -> (w a -> b) -> w b
  coreturn :: w a -> a
  coret    :: w a -> a
  cojoin   :: w a -> w (w a)
  x =>> f = fmap f (cojoin x)

{--Cojoin is a mess 
cojoining V x gives (V [V x] [x] [V x]) 
note that the focus is only one element
yet V x has a pair for its focus.
--}
instance Comonad V where 
   cojoin x = V (tail $ iterate lefty x) [x] (tail $ iterate righty x)
   coreturn (V _ [x] _ ) =  x
   coret (V _ [x,y] _ ) = x


--helpers
neg xs = map (* (-1)) xs
zeros = iterate id 0
loseit (V a b c) = [take 20 a, b ,take 20 c]
relax (V a b c) = V (stepdown a) (stepdown b) (stepdown c)
shift i v = (iterate (if i<0 then lefty else righty) v) !! abs i --whole 
shaft i v = (iterate (if i<0 then left else right) v) !! abs i --half
toList i j v = take (j-i) $ half $ shift i v where
    half (V _ b c) = b ++ c
stepdown [] = []
stepdown [[]]=[]
stepdown (x:xs) = x++stepdown xs
thatfunction n =  loseit((iterate (swapthatshit) naturalV) !!n)
toit = toList (-4) 12

------Comonadic Sort
samplesort = cSort (shift (-2) (randV 23)) 0

cSort v window =
          putStr $
          unlines $
          take 40 $ 
          map (show{--.dropWhile (==0)--}.toList window (window+20)) $
          iterate swapthatshit v

---rules
swapthatshit :: Ord x=> V x -> V x
swapthatshit = relax.(=>> swapleft).relax.(=>> swapright)
--swapthatshit = coreturn.(=>> swapleft).coreturn.(=>> swapright)
swap :: (Ord a) => V a -> [a]
swap (V a [b,c] d)| b<c = [b,c]
		  | otherwise = [c,b]
swapleft :: Ord x => V x ->  [x]
swapleft = (swap.shaft (-1)) 
swapright :: Ord x => V x -> [x]
swapright= (swap.shaft 1)


-------------Comonadic Prime Sieve
cPrimesieve1 =
          putStr $
          unlines $
          take 4000 $ 
          map (show.dropWhile (< 2).toList 0 50) $
          iterate sieveV (shift (-1) primeV)

cPrimesieve2 =
          putStr $
          unlines $
          take 4000 $ 
          map (show.dropWhile (< 2).takeWhile prime.
	  tail.tail.toList 0 50) $
          iterate sieveV (shift (-1) primeV)


primeV = V [1|k<-zeros] [2,3] [4..]

sieveV = relax.(=>> sieveleft).relax.(=>>sieveright)

sieveswap :: Integral a=> V a->[a]
sieveswap (V _ [1,a] _) = [1,a]
sieveswap (V _ [a,1] _) = [a,1]
sieveswap (V _ [b,c] _) | gcd b c ==1 = [c,b]
			| otherwise = [b]
sieveleft = (sieveswap.shaft (-1))
sieveright = (sieveswap.shaft 1)

--idea for random generator
differance :: Integral a=> [a]->[a]
differance [x] = []
differance (x:xs:xss) = (abs(x-xs)`div`2):differance (xs:xss)

randlist = qsort.differance.qsort

randput r =          
	  putStr $
          unlines $
          take 1000 $ 
          map (show.dropWhile (== 1).differance.toList 0 (r+1)) $
          iterate sieveV (shift (-1) primeV)



------------Comonadically Key Shuffling:
type LowNum = Int
--sampleshuffle = cShuffle (shift 0 zahlenV) 0
--comonadicshuffle v = cShuffle v 0
--navierStokes :: LowNum->LowNum->IO ()
--navierStokes j k = cShuffle (streakV j) k

--randomsV = randomV
--key v = zipV randomV v
-- zipV       
zipV :: V x-> V y-> V (x,y)
zipV (V a b c) (V x y z) = V (zip a x) (zip b y) (zip c z)
--projections
pr1V :: V (x,x) -> V x
pr1V (V a b c) = V [i|(i,j)<-a] [i|(i,j)<-b] [i|(i,j)<-c]
pr2V (V a b c) = V [j|(i,j)<-a] [j|(i,j)<-b] [j|(i,j)<-c]

--IO bitches
--cShuffle :: (Show y ,Ord y)=> V y -> Int -> IO ()
--cShuffle v window =
--          putStr $
--          unlines $
--          take 40 $ 
--          map (show.{--dropWhile (==0).--}toList window (window+30).pr2V) $
--          iterate swapthatshit (zipV randomV v)


--------------------------
{--
The goal is to lift (x=>>) to (W W a)
via fmap,     fmap (x=>>) (V [id..] id [id..])
How do I apply such a function?


:t (coreturn.fmap (naturalV =>>)) vids

(coreturn.fmap (naturalV =>>)) vids :: [V (V Int)]

--

 --some usage    map (head ids) [1..5]
functions f = f: functions f
ids = id: ids
coreturns :: [V a -> [a]]
coreturns = coreturn: coreturns

--supplying an argument to a list of functions
pam _ [] = []
pam a (f:fs) =  f a: pam a fs

vfunctions f = V (functions f) [f,f] (functions f)
vids = V ids [id,id] ids
vreturns = V coreturns [coreturn,coreturn] coreturns

{--
*Csort> (abyss.abyss.coreturn.fmap (naturalV =>>)) vids
[0,1]
*Csort> (coreturn.bayss.bayss.fmap (naturalV =>>)) vids
[0,1]
--}
abyss :: [V x]-> [x]
abyss = (coreturn.head)

bayss :: V x -> x
bayss = (head.coreturn) --}
