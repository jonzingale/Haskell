module Approximations (beanwildtownes,jrootk,streak,stirapprx,ecoclean) where
import Categoria
import System.Random
import Data.Char
import Data.List
import Primes

mkBlanket :: Int -> StdGen
mkBlanket cozy = mkStdGen cozy

--    kthRoot of j Approximation
ecoclean :: N->N->IO()       -- ~ j^(1/k)
ecoclean j k= let zs = (z10s 36 (jrootk j k)) in
     	do cls
	   (seqn[writeat (25,j) (show z) |(j,z)<-zip [0..24] zs])
	   putChar '\n'

--Integer "Decimal" Representation function
z10s :: N->[(Z,Z)]->[Z] 
z10s tol [] = []
z10s intro (duce:zs) = let (a,b)= re duce in 
 ((fst.divMod (a*10^intro)) b): z10s intro zs 

jrootk :: Z->Z->[(Z,Z)]
jrootk j k = 
	work (0,1)(j,1) 1 j   k
 where  work (a,b)(c,d) n jth kth
          | ltQ  (((freek kth).balky n)(comp(a,b)(c,d)))  (jth,1)=
              (a,b):work (balky n(comp(a,b)(c,d))) (c,d) (n+1) jth kth
          | otherwise                                            =
              (c,d):work (a,b) (balky n(comp(a,b)(c,d))) (n+1) jth kth

balky :: Z -> [(Z,Z)] -> (Z,Z)
balky n [(ad,xd),(cb,bd)] = --gives a random fraction between a pair.
     let l = 2^3 in
     re ( (fst.randomR(ad*l,cb*l))((mkBlanket.fromIntegral) n),   bd*l  )

-- re :: (Z,Z) -> (Z,Z) --reduces fractions.
-- re (a,b) = (a `div` (gcd a b), b `div` (gcd a b))

comp :: (Z,Z) -> (Z,Z) -> [(Z,Z)]    --Give two fractions
comp (a,b) (c,d) = [(a*d, b*d), (c*b,b*d)]--the same form.

ltQ :: (Z,Z) -> (Z,Z) -> Bool --  nonzero 
ltQ (a,b) (c,d) = a * d < c * b --determinant? 

freek :: Z->(Z,Z)->(Z,Z) -- (_^n)
freek 1 (a,b) = (a,b)  -- each component
freek n (a,b) = (a^n,b^n)


--     Random Streak  
streak ::Int->Int->Int->IO()      
streak spc joh dep = let zs = randomRs ((-1*spc),25) (mkBlanket 4) in
 do cls 
    (seqn[ j`writeat`(((int2let.spaces) z):"")    |(j,z)<-zip (scatter joh dep) zs])
    putChar '\n'
  where
     spaces z | z < 0 = -65
              | otherwise = z

scatter :: Int->Int->[(Int,Int)] --scattered coordinates
scatter a b = cohol a b ((split.mkBlanket) (lcm a b))
 where cohol x y (g,h) = let (s,t) = ( (fst.next) g, (fst.next) h) in
			 let (u,v) = ( (snd.next) g, (snd.next) h) in
	              (s`mod` a,t`mod` b):cohol a b (u,v)

{--}
let2int :: Char -> Int
let2int c = ord c - ord 'a'
int2let :: Int -> Char
int2let n = chr (ord 'a' + n)
--[int2let e| e<-[(-50)..(30)]]
-- a space is -65

--------- Show a list
splist ::(Show a)=>[a]->IO()      
splist as =   
     	do cls 
	   (seqn[ (j*2,25)`writeat` show a |(j,a)<-zip [0..length as] as])
	   putChar '\n'

----------Strings of ones in Prime Town: A meditation on probable factoring
{--
ones :: Z -> [Z]
ones 0 = []
ones n = 1:ones (n-1)
--}
moreones:: [Z]
moreones = 1:moreones

listtoZ :: [Z]->Z
listtoZ list = bolly list (size list)
 where
--  bolly ls 0 = 0
    bolly [] _ = 0
    bolly (l:ls) n = l*(10^(n-1))+bolly ls (n-1)
 
psofones :: [(Z,[Z] )] --Are all sequences of 1's > 11 composite?
psofones = [((listtoZ.take n) moreones,(ffactors.listtoZ.take n) moreones)|n<-[1..]]

ofones :: [Z]--all sequences of 1's 
ofones =[(listtoZ.take n) moreones|n<-[1..]]

takewhildones :: [(Z,[Z] )] --Are all sequences of 1's > 11 composite?
takewhildones = takeWhile (kittenpop ) [((listtoZ.take n) moreones,
		     (ffactors.listtoZ.take n) moreones)|n<-[3..]]
		where kittenpop (n,ffs) = size ffs > 2
beanwildtownes :: IO()    
beanwildtownes =   
  do cls 
     (seqn[(25,j*5)`writeat`(show fa++(spaces 100))|(j,(a,fa))<-zip [1..] takewhildones ])
     putChar '\n'

zeros :: Z->String
zeros 0 = ""
zeros n = (show 0)++(zeros (n-1))

spaces :: Z->String
spaces 0 = " "
spaces n = " "++(spaces (n-1))

--wildtwones ::IO()    
--wildtwones =   
--  do cls 
--     (seqn[(25,j)`writeat`show oandf|(j, oandf)<-zip [1..] probablyones ])
--     putChar '\n'

onesandafactor :: [(Z,Z)]
onesandafactor= [ (k,(ffactors k)!!1) | k<-tail ofones]

--probablyones :: [(Z,Bool,Z)]
--probablyones = [ (k,pGZA k,(pbfactors k)!!1) | k<-(tail.tail) ofones]

--beewhildones :: [(Z,[Z] )] --Are all sequences of 1's > 11 composite?
--beewhildones = takeWhile (kittenpop ) [((listtoZ.take n) moreones,
--		     (pbfactors.listtoZ.take n) moreones)|n<-[3..]]
--		where kittenpop (n,ffs) = size ffs > 2
--probableones :: [(Z,Int)]
--probableones = [ (p,(length.show) p) | p<-ofones, tol_GZa p 3]


{--
pbfactors :: Z->[Z]
pbfactors n = [ p | p<-takeWhile (< n) spitGZA, n`mod`p==0 ]  
--}
-----------

----------------

fuct :: Float->Float
fuct 0 = 1
fuct 1 = 1
fuct n  | n>1 = n* fuct(n-1)
	|otherwise = 1

(^^^) :: Float->Float->Float
(^^^) n 0 = 1
(^^^) n m = n*( n^^^(m-1))

stirapprx :: Integer ->(Integer, Float)
stirapprx n = (fact n, (intToFloat (n^n)) * exp(-(fromInteger n)))

stiraprx n =
     ( (log.fromInteger.fact) n,((fromInteger n)*(log.fromInteger)n)- fromInteger n)




