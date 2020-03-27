module Comonad (clPrimes,llsit,test,distance)where
import System.Random
import Data.Char
import Pprimes

type N = Integer
--cls = putStr "\ESC[2J"

data U x = U [x] x [x]

instance Show a => Show (U a) where
  show (U a b c) = show (a++[b]++c)

right (U a b (c:cs)) = U (b:a) c cs
left  (U (a:as) b c) = U as a (b:c)

instance Functor U where
  fmap f (U a b c) = U (map f a) (f b) (map f c)

class Functor w => Comonad w where
  (=>>)    :: w a -> (w a -> b) -> w b
  coreturn :: w a -> a
  cojoin   :: w a -> w (w a)
  x =>> f = fmap f (cojoin x)

instance Comonad U where
   cojoin a = U (tail $ iterate left a) a (tail $ iterate right a)
   coreturn (U _ b _) = b

---- distance for zippers
distance ::(Ord a) =>U a -> U a -> Int
distance u v  = let pairs = takeUntil (cotequal (coreturn v)) $ zip (map coreturn(iterate left u)) (map coreturn(iterate right u)) in
  (sign(coreturn v) (last pairs)) * ((length pairs)-1)

  where 
  cotequal c (a,b) = and [a/=c,b/=c]
  sign c (a,b) | c==a = 1
               | otherwise = -1
  takeUntil b xs= take (((length.takeWhile b) xs)+1) xs

dTo_x ::(Ord a) =>U a -> a -> Int
dTo_x  u a  = length $ takeWhile (cotequal a) $ 
   zip (map coreturn(iterate left u)) (map coreturn(iterate right u))
  where cotequal c (a,b) = and [a/=c,b/=c]

uptoN rule n = head $ drop (fromIntegral n) $ iterate ( =>> rule) seedU
-------

---Randomizing by cycles U [a]

keyU :: U [z] -> U [z]
keyU a = a =>> randi

--randi randomizes a U[z]
randi :: U [z] -> [z]
randi d =  let it = (length.coreturn) d in
  cycle_thr ((fst.randomR (1,it)) (mkBlanket it)) (coreturn d)
  where
    cycle_thr 0 xs = xs
    cycle_thr n (x:xs) = cycle_thr (n-1) (xs++[x])



-----

-- dTo_eleven :: Match -> Iterations -> IO()
dTo_eleven n us seed =
  do blosit lls seed (us+1);
     putChar '\n';
     putStr$ "distance till "++show n;
     putChar '\n';
     (putStr.show) $ dTo_x (uptoN lls us) n;
     putStr " from the center of the zipper"
     putChar '\n';
      
     
--rule (U (a:_) b (c:_)) = not (a && b && not c || (a==b))

shift i u = (iterate (if i<0 then left else right) u) !! abs i

toList i j u = take (j-i) $ half $ shift i u where
    half (U _ b c) = [b] ++ c

test = let u = U (repeat False) True (repeat False)
       in putStr $
          unlines $
          take 20 $
          map (map (\x -> if x then '#' else ' ') . toList (-20) 20) $
          iterate (=>> rule) u

---------------------------------------new code
--rules :: (Num a,Ord a) => U a -> a
rule (U (a:_) b (c:_)) = not (a && b && not c || (a==b))
pascal (U a b c) = b+ head a

lls (U x y z) = (head x + y + bang (U x y z)) `mod` 10
  where
    bang (U a b (c:cs)) | b + c < 9 = 0 
                        | b + c > 9 = 1 
                        |otherwise = (bang.right) (U a b (c:cs))

--Comonadic primeSieve !!!!
clPrimes n= [p |p<-cPrimes n,p/=1]
cPrimes m = (\m->toList 2 (m+1) $(=>> pSieve) nU) m       
pSieve (U (a:as) b c ) | a == 1 = b
                       | b `mod` a /= 0 = pSieve (U as b c)
                       | otherwise = 1

--Objs in a Comonad U
boolU   = U (repeat False) True (repeat False)
seedU   = U zeros 1 zeros
zU      = U (neg walk) 0 walk
nU  = U [1 | x <- walk] 1 walk
randU   = U (randomRs(0,9) (mkBlanket 43)) 2 zeros

--helpers
neg xs = map (* (-1)) xs
zeros = iterate id 0
digi = (intToDigit.abs.fromIntegral)

-- testit seedU rule3 computes the 11's trick
testit  u frule =
          putStr $ unlines $ take 100 $
          map (map (\x -> if x/='0' then x else ' ') 
          .goit. toList (0) 30) $ iterate (=>> frule) u

blosit rule u n = putStr $ unlines $ (take n) $
          map ((bloit.swdrp.toList (-80) n)) $ iterate (=>> rule) u


llsit = putStr $ unlines $ (take 30) $
          map ((bloit.swdrp.toList (-80) 50)) $
          iterate (=>> lls) seedU

swdrp = (reverse.(dropWhile (==0)).reverse.(dropWhile (==0)))

somefunction (x:xs) | x=='0' = somefunction (reverse xs)
                    | last (x:xs) =='0' = (somefunction.reverse.tail.reverse) (x:xs)
                    | otherwise = (x:xs)   
goit = show.twostr
 where
  twostr [] =  0
  twostr (n:ns) = (n*10^(length ns)) + twostr ns

bloit :: (Show a,Integral a) => [a]->String
bloit = twostr
 where
  twostr [] =  " "
  twostr (n:ns) = (show n) ++ twostr ns

 --some usage    map (head ids) [1..5]
functions f = f: functions f
ids = id: ids
coreturns :: [U a -> a]
coreturns = coreturn: coreturns


ufunctions f = U (functions f) f (functions f)
uids = U ids id ids
ureturns = U coreturns coreturn coreturns


{--
an attempt to apply a data
structure U of functions
over a data structure U
of values.

toList 0 15 $ coreturn (fmap (nU =>>) (ufunctions rule1))

nU is a U structure (U zeros 0 walk)
and ufunctions rule1 is a U structure
(U [rule1] rule1 [rule1]) which sums
the focus to its neighbor to the right.
All together the list of odd numbers is
returned.
--}

--natural to wordsized binary
n2b n = take 8 (f n)  
  where 
   f 0 = zeros  
   f n = mod n 2 : (n2b.div n) 2


rules n (U (a:as) b (c:cs)) 
  | [a,b,c] == [0,0,0] = (n2b n)!!0
  | [a,b,c] == [0,0,1] = (n2b n)!!1
  | [a,b,c] == [0,1,0] = (n2b n)!!2
  | [a,b,c] == [0,1,1] = (n2b n)!!3

  | [a,b,c] == [1,0,0] = (n2b n)!!4
  | [a,b,c] == [1,0,1] = (n2b n)!!5
  | [a,b,c] == [1,1,0] = (n2b n)!!6
  | otherwise          = (n2b n)!!7


--
ruleObj = 
 U [rules (mod n 256)|n<-neg [1..]] 
   (rules 0) 
   [rules (mod n 256)|n<-[1..]]


rulesObj = let (U a b c) = zU in
  U (map (rules.flip mod 256) a)
    (rules b)
    (map (rules.flip mod 256) c)
--}

lefts n x = (iterate left x)!!n
rights n x = (iterate right x)!!n


{--
throws a state at the ruleObj,
chooses rule 122 and prints to
the screen a portion of the 
resulting state.
 
toList (-3) 5 $(coreturn.(lefts 122).fmap (randU=>>)) ruleObj
--}


