module Test where
import System.Random
import Data.List

type Lattice = [[Z]]
type Z = Integer
type R = Float
type Base = Integer
type Prime = Z
type Rand = Z
type Cozy = StdGen

ones :: Z -> [Z]
ones 0 = []
ones n = 1:ones (n-1)

size :: [a]->Z  --size [[1,1]] = 1
size = foldr oneplus 0
       where oneplus _ n=n+1

qsort :: Ord a => [a]->[a]  
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
           where
             smaller = [s | s<-xs, s<=x]
             larger  = [l | l<-xs, l > x]

remdups :: Eq a => [a]->[a] 
remdups [] = []             
remdups (x:xs) = x:remdups (filter (/= x) xs)

xor :: (Eq a) => [a] -> [a] -> [a] --xor [1..5] [ j|j<-[1..10], even j] = [1,3,5,6,8,10]
xor birds bees = (union (nub birds) (nub bees)) \\ (intersect (nub birds) (nub bees))

cls :: IO()
cls = putStr "\ESC[2J"

fprimes :: [Prime]
fprimes = [p | p<-[2..],
  let xs = [1..(floor.sqrt.intToFloat) p] in 
   [ x  | x <- xs, p`mod`x == 0 ] == [1] ]

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

fact :: Z -> Z
fact 0 = 1
fact n = n*fact(n-1)
   
intToFloat :: (Integral a) => a -> Float
intToFloat n = fromInteger (toInteger n)

root ::Z -> Z
root  = (floor.sqrt.intToFloat)

loog :: Z->Z
loog = (floor.log.intToFloat)

baseList :: Z -> Base -> [Z] --baseList 4 2 = [0,0,1]
baseList 0 _ = []
baseList _ 0 = []
baseList n 1 = ones n
baseList n b = mod n b:(baseList.div n) b b 


--P.P Spitting--------------
prima :: Z
prima = 6271987

mkBlanket :: Int -> Cozy
mkBlanket cozy = mkStdGen cozy

--Probabablistic Prime Spitter "the RZA" or "the GZA" 
spit :: (Prime->Int->Bool)->[Prime]
spit hard = let rhyme = 7 in [ prime | prime <- [2..] , hard prime rhyme]

--method one
lil'toller :: Prime->Int-> Bool  
lil'toller red int = (and.take int) troofs == True
	where troofs = [ (a^red) `mod` red == a | a <-(map toInteger (clean red)) ] 
	
laban :: Int -> [Prime]
laban smit = [ p | p<-[2..], lil'toller p smit]

--method two (seemingly faster)
wran ::  Z -> Z
wran snug = (toInteger.fst.next.mkBlanket.fromIntegral) snug  

--Method three using rSort instead of clean
lil'ma :: Prime->Int-> Bool  
lil'ma red int = (and.take int) troofs == True
	where troofs = [ (a^red) `mod` red == a | a <-(rSort (fromIntegral red)) ] 

--Method four using iterative function hermit to make Z lists
pandaspit :: [Z]
pandaspit = [chops|chops<-[2..],panda chops 3 ==True]

panda :: Prime->Int->Bool
panda tex mex = purple tex mex []
 where purple tex mex xs = and[(la^tex)`mod`tex==la|la<-(hermit tex mex)]
       
------Random list of Ints
randlist :: Int -> [Int] --random list of Ints 
randlist n = take n (iterate (fst.next.mkBlanket)  ((fst.next.mkBlanket) n))

boredoms :: Int -> [Int] --Why not this instead of randlist?
boredoms zola = take zola (randoms (mkBlanket zola))

hermit :: Prime->Int->[Prime]
hermit p t = arcana [] p t 
 where arcana xs p t = 
	if length xs < t
	 then remdups((take t(randomRs(2,fromIntegral (p-1)) (mkBlanket t)))++xs)
	 else  xs

-------Random permutations of [a]
shuffle :: Ord a => [a]->[a]
shuffle xs = (snd.unzip.qsort)(zip ((boredoms.length) xs) xs)

------Randomly sorted list of Ints
tryagain :: Int -> [Int] --qsorting slows this shit down, but what else would work?
tryagain fool = (snd.unzip.qsort) (zip (boredoms fool) [1..fool]) 

rSort :: Int -> [Z] --random permutation of [1..n] (SLOWEST THING EVER)
rSort n = map snd ((qsort.zip  (randlist n)) [1..(fromIntegral n)]   )

clean :: Integral a => a -> [Int] --comparatively slow
clean loving = let icky = fromIntegral loving - 1 in
	take icky ((lookout.randomRs (1,icky)) (mkBlanket 42) )
	where
	lookout [] = []
	lookout (phaker:xs) = phaker:lookout [x|x<-xs, x/=phaker]
	-- suggest an n and receive a random permutation of the list [1..n-1]


-----Some Sorting Algorithms

{--randomized qsort: first randomize input list then qsort list
Why would doing qsort twice be faster than doing it just once? seriously, it does!
As a test try comparing: qsort [1..100000] versus rQsort [1..100000].
--}

rQsort :: Ord a => [a] -> [a]
rQsort eart = (qsort.snd.unzip.qsort) (zip (boredoms (length eart)) eart)

bubblesort :: (Ord a) => [a] -> [a] 
bubblesort xs = eather xs 0 0
 where eather ys s t = 
	if (ys!!s) <= (ys!!(s+1))
	then 
	  if t==(length ys)--see if list is done
	  then ys --yes, so return list
	  else eather ys ((s+1)`mod` ((length ys)-1)) (t+1)--move on down
	else eather ((take s ys) ++ [ys!!(s+1)] ++ [ys!!s] --switch'em
		++ (drop (s+2) ys)) ((s+1) `mod` ((length ys)-1)) 0 

trade :: [a]->Int->[a] --bubblesort and mergesort trade. qsort piles.
trade ys s = ((take s ys) ++ [ys!!(s+1)] ++ [ys!!s] ++ (drop (s+2) ys))  


mergesort :: (Ord a) => [a] -> [a]
mergesort clavinet = (stick.map (:[])) clavinet
 where stick [z] = z
       stick (x:y:zs) = 
         	if length (x:y:zs) == 1
	 	then  head (x:y:zs)
         	else stick (zs++[(merge x y)])	  

--given a pair of ordered lists, 
--merge returns a single ordered list.
merge :: Ord a => [a]->[a]->[a]
merge x y = dolly x y []
 where dolly [] [] z = z
       dolly a [] z  = z++a
       dolly [] b z  = z++b
       dolly (x:xs) (y:ys) z = 
	if x <= y
	then dolly xs (y:ys) (z++[x])
	else dolly (x:xs) ys (z++[y]) 
--------------------------


---Some powerful modulo throwing functions

rmod :: Z->Z->Z --Sooo much faster!!!
rmod a p = figit a p
 where figit b q =
	if (length.show) q > 4
	then let d = q`div`4 in
	     let r = q`mod`4 in
	      ((figit a d)^4)*((figit a r)) `mod` p
	else a^q`mod`p

--  moog (10^40000000) 234 has been computed, though just as fast as mod!
moog :: Z->Z->Z
moog a m = figit a m
 where figit b n =
	if (length.show) n > 4
	then let d = n`div`4 in
	     let r = n`mod`4 in
		((figit a d)^4)*((figit a r)) `mod` m
	else a `mod` m
	
pmod :: Z->Z->Z --Ball of mud. Work this out with more time.
pmod a p = (a^p) `mod` p


----Some modifications on some PPchecking favorites tol_GZa is unstoppable!!!

--variation on PPC using kmod. a bit faster than lil'ma 
--and without the segfaults. half as many randoms.
kilma :: Prime->Int-> Bool  -- Prime Tol -> Bool
kilma red int = (and.take int) troofs == True
	where troofs = [ rmod a red== a | a <-(rSort ((fromIntegral red)`div`2)) ] 

--rmod speeds up tol_RZa. Now good for values larger than 2^10000 !!!!!
tol_GZa :: Prime -> Int -> Bool 
tol_GZa toni tol = gza toni toni "tone" 
	where
	gza s p l
	  | kimchi ((s+2) `mod` p) p == False = False
	  | length l > tol = True
	  | otherwise = gza (wran s) p ('a':l)
          where kimchi a p = rmod a p == a `mod` p


--Sums corresponding components of two lists
(+++) :: (Num a) => [a]->[a]->[a]
[] +++ _ = []
_ +++ [] = []
(x:xs) +++ (y:ys) = (x+y):( xs +++ ys)

--------DOM->>COD

inhort ::(a->a)->[a]->[[a]] -- inhort (++"ap") ["p","t","s"] = ["pap","tap","sap"]
inhort g ys = f g ys 0
 where 
  f h xs n
    | n == length xs = []
    | otherwise = ( (take n xs)++[h(xs!!n)]++(drop (n+1) xs) ):(f h xs (n+1) )

cavort :: [a]->a->Int->[a]  --cavort [1,2,3] 5 1 = [1,5,3]
cavort xs a n = ( (take n xs)++[a]++(drop (n+1) xs) )

cohort :: Int->Int->[Z]-> [Z] --Nutrition function
cohort n m xs 
  | n==m = xs
  | otherwise = cavort (cavort xs (xs!!m + 1) m) (xs!!n - 1) n

whorsair :: [Z] -> [[Z]]
whorsair xs = xs: c xs [(n,m)|m<-[0..(length xs-1)],n<-[0..m], n/=m]
 where
  c ys (z:zs)
   | zs == [] = []
   | (cohort (fst z) (snd z) ys) /= (reverse.qsort) (cohort (fst z)(snd z) ys)= c ys zs 
   | otherwise = cohort (fst z) (snd z) ys :whorsair (cohort (fst z) (snd z) ys)++  c ys zs

suck :: [Z]
suck = [8,1,1,1]

-------- Cats & Randoms -------

dgnl :: a->(a,a)
dgnl f = (f , f)

pr1 :: (a , b) -> a
pr1 (f,g) = f

pr2 :: (a, b) -> b
pr2 (f,g) = g

	----- (f,g) X (j,k)
crossbow :: ((b->c),(y->z)) -> ((a->b),(x->y)) -> ((a->c),(x->z))
crossbow (f,g) (j,k) = ( (f.j),(g.k) )

	----- (f,g) (a,b) -> (fa, gb)
merzbow :: ((a->y),(b->z)) ->(a,b) ->(y,z)
merzbow (j,k) (n,m) = (j n, k m)
		---can merzbow and crossbow by one function?

	------spit pairs of randoms
rapiers :: Int->[(Int,Int)]
rapiers n = let it = (split.mkBlanket) n in
   [ bitchin' | bitchin'<-zip ((randoms.fst) it) ((randoms.snd)it) ] 

raptors :: Int->[(Int,Int)]
raptors rule = (uncurry zip) (merzbow (dgnl randoms) ((split.mkBlanket) rule))

fowl :: Int->(Int,Int)
fowl ball = (merzbow (dgnl (fst.next)) ((split.mkBlanket) ball))

swap :: [a]->Int->[a]
swap xs n = take n xs++[xs!!(n+1)]++[xs!!n]++drop (n+2) xs


	------Probabilistic Permutation Signature
rsgn ::Ord a=> [a]->[a]->String
rsgn xs ys = p xs 0 (mkBlanket 42)
   where 
    p list t r
      | length list /= length ys = "Lists have different sizes!" 
      | xs /= qsort list = "not equivalent lists!"
      | list == ys = if t`mod`2==0 then "( 1)" else "(-1)"
      | otherwise =  p (swap list (((`mod`(length xs-1)).fst.next) r))(t+1)((snd.next) r)

rsig ::Ord a=> [a]->[a]->Z
rsig xs ys = p xs 0 (mkBlanket 42)
   where 
    p list t r
      | list == ys = if t`mod`2==0 then 1 else (-1)
      | otherwise =  p (swap list (((`mod`(length xs-1)).fst.next) r))(t+1)((snd.next) r)

	------Finds the determinant on an nxn matrix of Z's
nxnzdet :: [[Z]]->Z
nxnzdet (x:xs) = 
 sum[(rsig [0..length x-1] q)*
      product[(x:xs)!!i!!(q!!i)|i<-[0..length x-1]]  |q<-(perms[0..length x-1])] 


perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map(interleave x) (perms xs))
  where interleave x [] = [[x]]
        interleave x (y:ys) = (x:y:ys):map (y:) (interleave x ys)

	-------Random nxn matrix
randtrix :: Int->Z->[[Z]]
randtrix seed n = star seed n 0 []
 where star s n t ys
        | t==n = ys
        | otherwise = [ r|r<-(take (fromIntegral n)(randomRs (-200,200) (mkBlanket s)))]
		:star ((fst.next.mkBlanket) s) n (t+1) ys

{--
WTF!!! 
[((`mod`4).fst.next.mkBlanket) n |n<-[0..]]
[((`mod`6).fst.next.mkBlanket) n |n<-[0..]]
--}
--Some names: exchequer, jack ketch, assay

--we would want values between 0 and 1 to sum to 1
-- floater :: Float->Float
-- floater a = (fst.randomR (0.0,1.0)) 
-- ((mkBlanket.truncate) a)
   
-- procession::Z->[Float]
-- procession 0 = [] 
-- procession n = (floater.fromIntegral) n:procession (n-1)

-- maria :: Float -> [Float]
-- maria rain = 
--       let doe = (fst.randomR (0.0,rain))
--                 ((mkBlanket.truncate) rain)in
--                 takeWhile (/= 0.0)((rain-doe):maria doe)

yogaPostures :: [String]
yogaPostures = ["downward Dog",
                "holdYourFootUpInFrontofYou",
                "halfLotusFold", 
                "headToKnee",
                "twists",
                "splits",
                "flowers",
                "pigeon",
                "camel"]

-- ralf.hutchison
-- Saturday June 18, 2011
-- diceBucket = qsort [mod n 191 | n <- [1,3..2151]]
-- countPartition :: Eq a => [a] -> [Int]
-- countPartition [] = []
-- countPartition xs = countPartition' 0 x xs

ralfablyPrim :: Integer
ralfablyPrim = 01234567891011121314151617181920212223242526272829303132333435363738394041424344454647484950515253545556575859606162636465666768697071727374757677787980818283848586878889909192939495969798991001011021031041051061071081091101111121131141151161171181191201211221231241251261271281291301311321331341351361371381391401411421431441451461471481491501511521531541551561571581591601611621631641651661671681691701711721731741751761771781791801811821831841851861871881891901911921931941951961971981992002012022032042052062072082092102112122132142152162172182192202212222232242252262272282292302312322332342352362372382392402412422432442452462472482492502512522532542552562572582592602612622632642652662672682692702712722732742752762772782792802812822832842852862872882892902912922932942952962972982993003013023033043053063073083093103113123133143153163173183193203213223233243253263273283293303313323333343353363373383393403413423433443453463473483493503513523533543553563573583593603613623633643653663673683693703713723733743753763773783793803813823833843853863873883893903913923933943953963973983994004014024034044054064074084094104114124134144154164174184194204214224234244254264274284294304314324334344354364374384394404414424434444454464474484494504514524534544554564574584594604614624634644654664674684694704714724734744754764774784794804814824834844854864874884894904914924934944954964974984995005015025035045055065075085095105115125135145155165175185195205215225235245255265275285295305315325335345355365375385395405415425435445455465475485495505515525535545555565575585595605615625635645655665675685695705715725735745755765775785795805815825835845855865875885895905915925935945955965975985996006016026036046056066076086096106116126136146156166176186196206216226236246256266276286296306316326336346356366376386396406416426436446456466476486496506516526536546556566576586596606616626636646656666676686696706716726736746756766776786796806816826836846856866876886896906916926936946956966976986997007017027037047057067077087097107117127137147157167177187197207217227237247257267277287297307317327337347357367377387397407417427437447457467477487497507517527537547557567577587597607617627637647657667677687697707717727737747757767777787797807817827837847857867877887897907917927937947957967977987998008018028038048058068078088098108118128138148158168178188198208218228238248258268278288298308318328338348358368378388398408418428438448458468478488498508518528538548558568578588598608618628638648658668678688698708718728738748758768778788798808818828838848858868878888898908918928938948958968978988999009019029039049059069079089099109119129139149159169179189199209219229239249259269279289299309319329339349359369379389399409419429439449459469479489499509519529539549559561

-----------SquareRoot of Two Approximatey
re :: (Z,Z) -> (Z,Z)
re (a,b) = (a `div` (gcd a b), b `div` (gcd a b))

ltQ :: (Z,Z) -> (Z,Z) -> Bool
ltQ (a,b) (c,d) = a * d < c * b

fqr :: (Z,Z) -> (Z,Z)
fqr (a,b) = (a*a,b*b)

comp :: (Z,Z) -> (Z,Z) -> [(Z,Z)]
comp (a,b) (c,d) = [(a*d, b*d), (c*b,b*d)]

balky :: Z -> [(Z,Z)] -> (Z,Z)
balky n [(ad,xd),(cb,bd)] = 
     let l = 10^1 in 
     re ( (fst.randomR(ad*l,cb*l))((mkBlanket.fromIntegral) n),   bd*l  )
lithow ::[(Z,Z)]
lithow = work (0,1)(2,1) 1
 where work (a,b)(c,d) n
          |ltQ((fqr.balky n)(comp(a,b)(c,d)))(2,1)=
              (a,b):work(balky n(comp(a,b)(c,d)))(c,d)(n+1)
          |otherwise =
              (c,d):work (a,b) (balky n(comp(a,b)(c,d))) (n+1)

--------
flo :: (Z,Z)->Float
flo (a,b)= (fromIntegral a)/(fromIntegral b)

heathen::[Float]
heathen = [ flo w |w<-lithow]




----- coefficients in 7's tricks

ps_and_ks = takeWhile ((<1000).(fst)) [(p,k)|p<-fprimes,k<-[1..p],(10*k)`mod`p==1]

p_cycles :: Z -> Z -> [Z]
p_cycles n k = 

  {-- 
    check if n yields a decision for k
      1) divmod by 10 on n to get a head and tail
      2) multiply tail by k
      3) sum the two (or subtrack additive inverse) and take absolute value
      4) iterate

















