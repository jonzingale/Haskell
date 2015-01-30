module Randmatrix where
import System.Random
import Data.List
import Primes
import Categoria

--An idea regarding random matrices. .
type PhiTheta = (Float,Float)
type R3 = (Float,Float,Float)
type R2 = (Float,Float)
type Z3=(Z,Z,Z)
type Z2=(Z,Z)
type Rate=Int
type Epsilon=Z

mkBlanket :: Int -> StdGen
mkBlanket cozy = mkStdGen cozy

--example_sphere = (animate 5 2 qsphere) (randtrix 4 20)
animate :: (Show a)=>Rate->Epsilon->( [[a]]->([[a]],[R3]) )->[[a]]->IO()
animate r eball f matrix= --qsphere is an example of such a second argument.
     do cls               -- less obvious is (imbed2to3.prj1.torus.coordup)
	miller r eball f matrix
 where miller r lite f m=
        do 
	(seqn[(3,j)`writeat`show x|(j, x)<-zip [1..] m ])
	wait (10^r)
	miller r lite f (((modout lite).f) m)

wild3 :: (Show a,Show b)=> [a]->[b]->IO()
wild3 xs ys=
	do cls
	   (seqn[(3,j)`writeat`show x|(j, x)<-zip [1..] xs ])
	   (seqn[(3,j+length xs+1)`writeat`show x|(j, x)<-zip [1..]ys ])
           putChar '\n'
--for example: (\m->((wild3 m).(modout 1).qsphere) m) [[a]]

--Test functions
fave :: [[Z]]
fave =  [[0,0,0],[1,1,1],[2,2,2],[3,3,3],[4,4,4]]
tele :: [[Z]]
tele = [[1,2,3],[4,5,6],[7,8,9]]
sixer :: [String]
sixer =  ["abc123","def456","ghi789","jklyz0","mnovwx","pqrstu"]
alphab = ["abcdef","ghijkl","mnopqr","stuvwx","yz1234","567890"]
rand3::Int->[Z3]
rand3 sd = let cs s=take 20 (randomRs (0,9) (mkBlanket (sd*s))) in
	[(x,y,z)|(x,(y,z))<-zip (cs 1987) (zip (cs 1988) (cs 1989))]
trusted ::[Z3]
trusted = pony 0 where pony n= (n,n,n):pony (n+1)
trusty :: [R3]
trusty = [(fromIntegral x,fromIntegral y,fromIntegral z)|(x,y,z)<-trusted]
qsphere :: [[a]]->([[a]],[R3])
qsphere as = (sphere.torus.coordup) as
nextus :: [Int]
nextus = jobin 420
 where jobin s= ((fst.next.mkBlanket)s)`mod`13:jobin ((fst.next.snd.next.mkBlanket) s)
randtrix ::(Num a,Random a,Integral n)=> Int->n->[[a]]
randtrix seed n = star seed n 0 []
 where star s n t ys
        | t==n = ys		---Random nxn matrix
        | otherwise = [ r|r<-(take (fromIntegral n)(randomRs (0,9) (mkBlanket s)))]
		:star ((fst.next.mkBlanket) s) n (t+1) ys
randbitrix ::(Integral n)=> Int->n->[[Char]]
randbitrix seed n = star seed n 0 []
 where star s n t ys
        | t==n = ys		---Random nxn matrix of ' ` ' and ' '.
        | otherwise = [ ['`',' ']!!r|r<-(take (fromIntegral n)(randomRs (0,1) (mkBlanket s)))]
		:star ((fst.next.mkBlanket) s) n (t+1) ys

-------

--left and right fatty joints
coordup:: [[a]]->( [[a]], [Z2])
coordup matrix=(matrix,coords matrix)
forget :: ( [[a]],[b]) -> [[a]]
forget (a,b) = a
-----
-----Operators
imbed2to3 :: ([[a]],[R2])->([[a]],[R3])
imbed2to3 (a,ts) = (a ,[(i,j,0)|(i,j)<-ts])

disk :: ([[a]],[Z2])->([[a]],[R2])
disk (a,zs) = (a,[(fromIntegral i,fromIntegral j)|(i,j)<-zs])

torus :: ([[a]],[Z2])->( [[a]], [PhiTheta])
torus (matt,zs) =  let (r,c)=getmXn matt in
		   let ts=[(fromIntegral i,fromIntegral j)|(i,j)<-zs] in
   (matt, [( (i*2*pi)/(fromIntegral r),(j*2*pi)/(fromIntegral c))|(i,j)<-ts] )
sphere :: ( [[a]], [PhiTheta])->( [[a]], [R3])
sphere  (m,phithetas) = (m, [(ze(cos y*sin x),ze(sin y*sin x),ze(cos x))|(x,y)<-phithetas])          where
	 ze r | and[(r< 0.01),(r> -0.01)] = 0
     	      | otherwise = r
prj1 :: ([[a]],[R2])->([[a]],[R2])
prj1 (a,zs) = let props = (fst.unzip)zs in
           (a, [( p,fromIntegral zero)|(p,zero)<-zip props zeros])
prj2 :: ([[a]],[R2])->([[a]],[R2])
prj2 (a,zs) = let props = (snd.unzip)zs in
           (a, [( p,fromIntegral zero)|(p,zero)<-zip props zeros])
--------

--helpers
godout :: Z->( [[a]],[R3])->[(a,[Z])]
godout eball (m,t) = goods
   where
   fbrs  = fibres (eball*10^4) (toZ3 5 t)   
   arts  = zip (fallout m) (toZ3 5 t) 
   goods = [(a,fs)|(a,cs)<-arts,(fs,ps)<-fbrs,cs==ps]
   gunho s [] ms= ms
   gunho s ((a,zs):ls) ms=       let (i,g)=(next.mkBlanket)s in
	gunho ((fst.next)g) ls (replace (i`mod`length zs) a ms) 
modout :: Z->( [[a]],[R3])->[[a]]
modout eball (m,t) = chop ((length.head) m) (gunho 13 goods (fallout m))
   where
   fbrs  = fibres (eball*10^4) (toZ3 5 t)   
   arts  = zip (fallout m) (toZ3 5 t) 
   goods = [(a,fs)|(a,cs)<-arts,(fs,ps)<-fbrs,cs==ps]
   gunho s [] ms= ms
   gunho s ((d,zs):ls) ms=       let (i,g)=(next.mkBlanket)s in
	gunho ((fst.next)g) ls (replace (zs!!(i`mod`length zs)) d ms) 
replace:: (Integral b)=>b->a->[a]->[a]
replace n x xs= take (fromIntegral n) xs++[x]++(drop (fromIntegral (n+1)) xs) 
fibres::Z->[Z3]->[ ([Z],Z3) ]
fibres tol xs = let js= zip walk xs in
 cake tol js      
  where
   cake t [] = []
   cake t ((j,a):cs) = ( [j|(j,b)<-((j,a):cs) ,close tol a b],  a):
                 cake t  [(j,b)|(j,b)<-cs ,(not.close tol a) b]
close :: (Num a,Ord a)=>a->(a,a,a)->(a,a,a)->Bool
close e (a,b,c) (x,y,z)= and[abs(a-x)<e,abs(b-y)<e,abs(c-z)<e]
toZ :: (Integral a)=>a->R->Z
toZ tol t = floor (t * (10^^tol))
toZ3:: (Integral a)=>a->[R3]->[Z3]
toZ3 _ [] = [] --truncated Floats as Z's
toZ3 tol ((x,y,z):ts)= (toZ tol x,toZ tol y,toZ tol z):(toZ3 tol ts) 
nip:: ([[a]],[b])->[(a,b)]
nip (a,b) = zip (fallout a) b
zeros ::[Z]
zeros = 0:zeros  
fallout [] = []
fallout (p:ps) = p++fallout ps
chop :: Int->[a]->[[a]]
chop n [] = []
chop n ts = take n ts:chop n (drop n ts)
getmXn :: [[a]]->Z2
getmXn (x:xs) = (size (x:xs),size x)
coords :: [[a]]->[Z2]
coords (x:xs)= let rows =[0..(size (x:xs)-1)] in 
	       let columns = [0..(size x -1)] in
	          [(i,j)|i<-rows,j<-columns]
ctrans :: ([[a]],[Z2])->([[a]],[Z2])
ctrans ((a:as),z) = (transpose (a:as),[(j,i)|(i,j)<-(fallout.transpose.chop(length a))z]) 
{--
In the future, consider switching
the coordinates in reverse as a method
of getting back to data of matrix type.
ie: R3->PhiTheta->Z2 

write some more cool mappings!
projective plane
klein bottle
other characterizations of the sphere
projections
torus that identifies endpoints of square
 (as opposed to making endpoints adjacent).
torus in 3 space ie: R(cos x*sin y) ect..

make a quotient space maker

eball of fibers and toZ3's tolerance 
need to be sync'd up somehow.

fibers of torus? what about fibers 
across composition?

try: (\m->wild3 (((godout 1).qsphere)m) (((modout 1).qsphere)m)) [[a]]
--}


