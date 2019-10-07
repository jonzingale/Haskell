module Categoria where
import System.Random
import Data.List
import Primes

--type Z = Integer
type N = Integer
type R = Float
type Domain = Z
type Codomain = Z
type Binary = (N->Unary)

-------- Cats & Randoms -------
dgnl :: a->(a,a)
dgnl f = (f , f)

pr1 :: (a , b) -> a
pr1 (f,g) = f

pr2 :: (a, b) -> b
pr2 (f,g) = g

  ----- (f,g) (a,b) -> (fa, gb)
merzbow :: ((a->y),(b->z)) ->(a,b) ->(y,z)
merzbow (j,k) (n,m) = (j n, k m)
    ---can    merzbow   &  crossbow be one fun ction?
  ----- (f,g) X (j,k)
crossbow :: ((b->c),(y->z)) -> ((a->b),(x->y)) -> ((a->c),(x->z))
crossbow (f,g) (j,k) = ( (f.j),(g.k) )


---------------Graphing a Function
---t parameterized family of functions
--  [f 7   |f<-(listingf (*) 5 13)]
tlisting :: Binary -> N ->N -> [Unary]
tlisting f a b = map (famt f) [a..b]
    where famt f t = f t

iteratedf :: Unary->Int->[Unary]
iteratedf f n = take n (iterate id f)

--Unaries
two :: Unary
two n = 2^n

tre :: Unary
tre m = 3^m

constan :: Unary
constan _ = 5

-- primely :: Unary
-- primely 0 = 0
-- primely n = fprimes!!(fromInteger (n-1))

-- fact n | n<2 = 1
--  | otherwise = n*fact(n-1) 

-- size ::  [a]->Integer
-- size [] = 0
-- size n = 1 + (size.tail) n


fib :: Unary -- fib n = nth fibonacci number
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n+1)

--life (nry 3) (nry 2) 3 3
--life (nry 5) (nry 5) 4 4

-------Binaries
nry :: Binary
nry a b = a ^ b

comb :: Binary
comb n 0 = 1
comb 0 n = 1
comb n k = div (fact n) (fact k * fact (n-k))

monos :: Domain -> Codomain -> Z
monos d c = if d<=c then fact d * comb c d else 0
             
retractions :: Domain -> Codomain -> Z
retractions d c = if d <= c then d^(c-d) else 0

stirling :: Domain->Codomain->Z
stirling d c = sum[(-1)^k*(comb c k)*(c-k)^d | k<-[0..c]]`div`(fact c)
  where comb n k = div (fact n) (fact k * fact (n-k))

epis :: Domain-> Codomain -> Z
epis d c = sum [(-1)^k * (comb c k) * (c-k)^d | k <- [0..c]]
  where comb n k = div (fact n) (fact k * fact (n-k))

---------
fourin :: (N->N)->(N->N)->N->N->([N],[N])
fourin f g n m = (map f [0..n] ,map g [0..m] ) 

xyplanet :: ([a],[a])->[(a,a)] --formerly scifi
xyplanet ([],_) = []
xyplanet ((x:xs),ys) = [(x,y)|y<-ys]++xyplanet (xs, ys)

zaxist :: Num a =>([a],[a]) -> [a] --the multiplication f(a)
zaxist ([],_) = []
zaxist ((x:xs),ys) = [x*y|y<-ys]++zaxist (xs, ys)

-----mappings
      --graph in radians
rempet ::  Unary->Unary->N->N->[ ((R,R) , N)]
rempet f g n m  = let xsys = fourin f g n m in
   zip ((xyplanet.picoord) xsys) (zaxist xsys) --send this to IO
      --graph in inches
nempet ::  Unary->Unary->N->N->[ ((N,N) , N)]
nempet f g n m = let xsys = fourin f g n m in
   zip (xyplanet xsys) (zaxist xsys) --send this to IO

type RCoord = ( [N],[N])->( [R],[R])
type Euclid = ( [R],[R])->([R],[R])
picoord :: RCoord
picoord (xs,ys) = let bo = map fromInteger xs in
                  let po = map fromInteger ys in
    let lb = fromInteger (size xs) in
    let lp = fromInteger (size ys) in
    ( [2*pi*k/lb |k<-bo] , [2*pi*k/lp |k<-po] ) 

      ---------IO Monad Grapher---
width :: Int
width = 50
height :: Int
height =30

type Board = [Pos]
type Pos = (Int,Int)

cls :: IO()
cls = putStr "\ESC[2J"

writeat :: Pos -> String -> IO()
writeat p xs = do goto p
                  putStr xs

goto :: Pos -> IO()
goto (x,y) = putStr ("\ESC[" ++ show y++";"++show x++"H")

seqn :: [IO a]-> IO()
seqn[] =return()
seqn (a:as) = do a
                 seqn as

type Unary = N->N
xyBoard :: Unary->Unary->N->N->Board --Type Board = [(Int,Int)]
xyBoard f g n m = (tumeric.xyplanet.fourin f g n)m

tumeric :: [(N,N)]->Board
tumeric [] = []
tumeric ((p,q):ps) = (fromInteger p, fromInteger q):tumeric ps 
----

--------Models
type Model = Unary->Unary->N->N->IO()
scalar :: Integral a=>Int->(a,a)->(Int,Int)
scalar r (a,b) = (r*(fromIntegral a),(2*r*(fromIntegral b))`div`5)

walk ::(Enum a,Num a)=> [a]
walk = [0..]

ploopta :: Integral a =>a->a->[(a,a)]
ploopta 0 b = [(0,k) | k<-[0..b]]
ploopta a b = [(a,k) | k<-[0..b]] ++ ploopta (a-1) b


--------
compo :: N->[Unary]->[Unary]->IO()
compo n fs gs =
  seqn[writeat (scalar 10 p) (val p n) |
  p<-ploopta (length fs-1)(length gs-1)]
        where
  val (x,y) t = show ( ((gs!!y).(fs!!x)) t)

cuntruff :: N->[Unary]->[Unary]->IO() --composary table
cuntruff n fs gs = 
  do cls   --cuntruff 1 (tlisting (*) 1 14) (tlisting (monos) 1 14)
     compo n fs gs
     putChar '\n'

-------
classico :: Binary->Model
classico b f g x y =
  seqn[writeat (scalar 10 p) (val f g p) |
  p<-ploopta x y]
        where
  val l r (p,q) = show (b (l p)  (r q))
classie :: Binary -> Model
classie b f g x y =                     --Classic table
     do cls
        classico b f g x y
        putChar '\n'

gennesse :: (Show c)=> (a->b->c)->(N->a)->(N->b)->N->N->IO()
gennesse b f g x y = 
  seqn[writeat (scalar 15 p) (val f g p) |
  p<-ploopta x y]
        where
  val l r (p,q) = show (b (l p)  (r q))
generoll :: (Show c)=> (a->b->c)->(N->a)->(N->b)->N->N->IO()
generoll b f g x y=
  do cls
     gennesse b f g x y
     putChar '\n'
{--
I would like to make this program a bit
closer to grluck in format. nice and clean spacing of primes
--}
-- primetyme :: IO()
-- primetyme = let primers =map ((++" ").show) (takeWhile (<10000) fprimes) in
--      do cls
--   seqn[writeat (scalar 6 (w `mod` 30,5+w`div`30)) p |(p,w)<-zip primers walk]



 
-------
      
index :: [a]->N->N->[(N,a)]
index list a b = [ (i,list!!(fromIntegral i)) |i<-[a..b] ]

graphLxK :: (Show c)=>(a->b->c)->Int->N->N->N->N->[a]->[b]->IO()
graphLxK bry scal a c x z as xs =
  let axisa = index as a c in
  let axisx = index xs x z in
  let body = grphprod bry axisa axisx in
  do cls
     seqn [writeat (scalar scal p) (show q) | (p,q)<-body]  
     putChar '\n'
grphprod :: (a->b->c)->[(N,a)]->[(N,b)]->[( (N,N),c) ]
grphprod b [] _ = []
grphprod b ((i,fx):ns) ps =
       [ ( (i,j), fx`b`gy)| (j,gy)<-ps] ++ grphprod b ns ps

-----doubled code.. write function f -> (0,10,0,10)
grluck :: (Show c)=>(a->b->c)->[a]->[b]->IO()
grluck bry as xs =
  let axisa = index as 0 10 in
  let axisx = index xs 0 10 in
  let body = grprod bry axisa axisx in
  do cls
     seqn [writeat (scalar 2 p) (show q) | (p,q)<-body]  
     putChar '\n'
grprod ::(a->b->c)->[(N,a)]->[(N,b)]->[( (N,N),c) ]
grprod b [] _ = []
grprod b ((i,fx):ns) ps =
       [ ( (i,j),(b fx) gy)| (j,gy)<-ps] ++ grphprod b ns ps

twist :: (a,b)->(b,a)
twist (a,b) = (b,a)

-------WTF!!!!!!!!!!!
epis' :: Z-> Z -> Z
epis' d c = sum [((-1)^k * (comb c k) * (c-k)^d) | k <- [0..c] ] 
  where comb n k = div (fact n) (fact k * fact (n-k))

epinski :: Z->Z->Z->Z
epinski m d c = epis' d c `mod` m

combinski :: Z->Z->Z
combinski d c = comb d c `mod` 2

stirpinski :: Z->Z->Z
stirpinski d c = stirling d c `mod`2 
--grluck with indices over 300a 100x
--and the scaling very low ~ 1
--sierpinski
--mudepi is considerably faster


{--
graphLxK (*) 0 10 0 10 (ffactors (2^23)) fprimes
graphLxK (monos) 0 10 0 10 [0..] [0..]
--}

graphmoney :: Binary->N->IO()
graphmoney binary count=
  do cls
     graphLxK binary 9 0 10 0 10 [count..] [count..]
     putChar '\n'
     wait (2*5^8)
     graphmoney binary (count+1)

wait :: Int -> IO()
wait n = seqn [return () | _ <- [1..n]]


-------- mu function fun and the wrinkly curtain
-- mu :: N->N
-- mu n| (not.sqrfree) n = 0
    -- | (odd.length.pfactors) n = -1
    --  | otherwise = 1
-- 
-- it shouldn't be necessary to verify with all pfactors
-- sqrfree :: N->Bool
-- sqrfree n = and[fpsinnum n p==1|p<-pfactors n]
 -- This code should make things faster but doesnt, well
 --  only when there are already many squares involved.
-- sqrfree :: N->Bool
-- sqrfree n = sf n 2
 -- where
  -- sf a b| a`mod`(b^2)==0 = False
  -- | and [b<a , a`mod`(b^2)/=0] = sf a (b+1)
  -- | otherwise = True
-- }
-- divides ::N->N->Bool
-- divides a b| let f=fromIntegral in
  -- (f b)/(f a)==f(b`div`a)=True
     -- | otherwise = False
-- diV :: N->N->N
-- diV b a | a`divides`b = b`div`a
  -- | otherwise = 0
-- 
-- mulatto :: N->N->N
-- mulatto b a |a`divides`b=(mu.diV b) a
      -- |otherwise = 0

-- nuluck :: N->IO()
-- nuluck n =
 -- let ringy ds = (\ds->["o *"!!((fromIntegral.(+ 1).mulatto (n`diV`ds)) d)|d<-ffactors n]) ds in
     -- do cls
  -- seqn[writeat (0,i) (ringy q)| (i,q)<- zip [0..] ((reverse.ffactors) n)]
  -- putChar '\n'
 -- "_ |" ,  "o *"
-- spongy :: N->String->IO()
-- spongy n xs=
 -- let ringy ds = (\ds->[xs!!((fromIntegral.(+ 1).mulatto (n`diV`ds)) d)|d<-ffactors n]) ds in
     -- do cls
  -- seqn[writeat (0,i) (ringy q)| (i,q)<- zip [0..] ((ffactors) n)]
  -- putChar '\n'
-- some favorite seeds
-- sample :: Int->N
-- sample n = product(take n fprimes)

-- randuck n = (\n->nuluck ((fromIntegral.fst.next) (mkBlanket n))) n
-- duck n m=(\m n->((fromIntegral.fst.next)(mkBlanket m))
               -- *((fromIntegral.fst.next)(mkBlanket n))`mod`(2^20)) n m
-- randuck 5,34,32,36,2123 are good!
-- (nuluck.duck) for 5,4 5,7 2123,24 27,4209
-- two numbers with 64 factors: 7560 9240
-- wrinkle 4 12 !!!!<----- paint this one 

{--
Future Ideas regarding wrinkle curtain program.

*work on efficiency.
 large numbers are beautiful
 but way too slow to compute
 there might be places with 
 redundant factoring and the
 like.
*nuluck (product(take 7 (drop 5 fprimes)))<----- --paint this one
*make randomlists of primes.
*make characters variable <--spongy
*permute factors. factors are a set, no structure is implied.
--}

--random sorts
--boredoms :: Int -> [Int]
--boredoms zola = take zola (randoms (mkBlanket zola))
--
--shuffle :: Ord a => [a]->[a]
--shuffle xs = (snd.unzip.qsort)(zip ((boredoms.length) xs) xs)
--
--wrinkle :: Int->Int->IO()
--wrinkle l n = let aurora xs= (snd.unzip.qsort)(zip (boredoms n) xs) in
 --(nuluck.product.(take (l+5)).(drop l).aurora) fprimes

{--
I would like to write nuluck to a txt file for printing.
banner =do
  writeFile "Wonderful2.doc" (map id inpStr)
--}




--more true luck

{--RSA groups!!!

the RSA condition:
ka * kb == 1 mod phi(m), (a,m)=1
=> a^(ka * kb) == a mod m

--}
rsaGroup m = (\m-> (grrluck (rsap m)
  [a |a <-[1..m],gcd a (totient m)==1] -- [1,3,5,7,9] for m =10
  [a |a <-[1..m],gcd a (totient m)==1]) ) m
rsaGrp m = (\m->grrluck (rsap m) (take (fromIntegral m) walk) (take (fromIntegral m) walk)) m
rsaGrp2 m = (\m->grrluck (rsaq m) [a |a<-[1..m],gcd a (totient m)==1 ] [a |a<-[1..m],gcd a (totient m)==1 ])m

rsa m k a b = ((a^k `mod` m) *  (b^k `mod` m)) `mod` m
rsap m ka kb = (ka * kb) `mod` (totient m) 
rsaq m ka kb = (ka * kb) `mod` m 

-- totient :: N->N
-- totient n = sum[1 |k<-[1..n], gcd n k==1]

grrluck :: (Show c)=>(a->b->c)->[a]->[b]->IO()
grrluck bry as xs =
  let axisa = index as 1 ((size as)-1) in
  let axisx = index xs 1 ((size xs)-1) in
  let body = grprod bry axisa axisx in
  do cls
     seqn [writeat (scalar 4 p) (show q) | (p,q)<-body]  
     putChar '\n'
--rsa group tables mother lover. ring of units
duckett m = -- <--- this is probably what I wanted above at rsaGroup
  grrluck (rsap m) 
   (1:[a |a<-[0..((totient m)`div`1)],gcd a (totient m)==1])
     (1: [a |a<-[0..((totient m)`div`1)],gcd a (totient m) == 1])
