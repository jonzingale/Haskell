module Marsden (vectorsum,scaleby,scalarproduct,crossproduct,crosstring,randtrix,nxndet,cls,norm,norhmm,normalize,angleradians,angledegrees,isorthogonal,components,projections,parallarea,parallvol,mXm,idMatx,scaleMatx)where
import System.Random
import Data.List
import Primes
import Categoria
import SortsShuffles
{--
type R = Double
--} 

{--
Adding Vectors
Scaling a Vector

Scalar Product
Cross Product in 3-dim
Cross Product (String)
Triple Product

Norm (or Length) of a Vector
Is a Vector a norm?
Normalize a Vector
Angle between two Vectors
Orthogonal Vectors?
Component c along a
Projection of c onto a

Area of a paralellogram with adjacent sides a b
Volume of a parallelepiped formed by a b c

Matrix Multiplication AB
Scaling a Matrix
Determinant of a Matrix
Generate a Random Matrix
Identity matrix size n
--}

vectorsum ::Num a=> [a]->[a]->[a]
vectorsum xs ys = [ x+y | (x,y)<-zip xs ys]
scaleby :: Num a => a->[a]->[a]
scaleby t vect= map (* t) vect

scalarproduct :: Num a=> [a]->[a]->a
scalarproduct xs ys = sum[ x*y | (x,y)<- zip xs ys]
crossproduct :: Num a=> [a]->[a]->[a]
crossproduct (a:[b]) (x:[y]) = crossproduct [a,b,0] [x,y,0]
crossproduct (a:b:[c]) (x:y:[z]) = [(b*z-c*y),(a*z-c*x),(a*y-b*x)]	
crosstring :: (Show a,Num a)=> [a]->[a]->String
crosstring (a:b:c:[]) (x:y:z:[]) = 
        show (b*z-c*y)++"i+ "++show (a*z-c*x)++"j+ "++show (a*y-b*x)++"k"
tripleproduct :: Num a=> [a]->[a]->[a]->a
tripleproduct a b c = scalarproduct a (crossproduct b c)

norm :: [R]->R
norm a = (sqrt.scalarproduct a) a
norhmm :: [R]->Bool
norhmm a = norm a==1
normalize :: [R]->[R]
normalize t = scaleby (1/(norm t)) t
angleradians :: [R]->[R]->R
angleradians a b =  acos ((scalarproduct a b)/((norm a)*(norm b)))
angledegrees a b = angleradians a b * 360 / (2*pi)
anglestring :: [R]->[R]->String
anglestring a b = show (realtopair(angleradians a b/pi))++" pi"
isorthogonal :: (Num a,Eq a)=>[a]->[a]->Bool
isorthogonal x y |or [vectorsum x y==x,vectorsum x y==y] = False 
		 |otherwise= scalarproduct x y == 0
components :: [R]->[R]->[R]->(R,R)
components a b c | isorthogonal a b = --how can we test that c is in the plane?
   (scalarproduct a c/(scalarproduct a a),scalarproduct b c/(scalarproduct b b))
		 | otherwise = (0,0)
projections :: [R]->[R]->[R]->([R],[R]) --This is also unclear. review and return.
projections a b c =(scaleby ((fst.components a b)c) a,scaleby ((snd.components a b)c) b)

parallarea :: [R]->[R]->R
parallarea a b = (norm.crossproduct a)b
parallvol  :: [R]->[R]->[R]->R
parallvol a b c = (abs.scalarproduct c) (crossproduct a b)

{--to Consider:
parametrics
 -the line
 -the plane
projections
rref
rotate a vector
--}

-----
mXm :: Num a=>[[a]]->[[a]]->[[a]]
mXm [] b = []
mXm (a:as) bs = let bt = transpose bs in [scalarproduct a b|b<-bt]:mXm as bs
sumMx :: Num a=>[[a]]->[[a]]->[[a]]
sumMx [] b = []
sumMx (a:as) (b:bs) = vectorsum a b:(sumMx as bs) 
rotmatxy :: R->[[R]]->[[R]] --needs work check Sadun or MacLane
rotmatxy ang vect= (blur.mXm vect) [[sin ang,cos ang]]

scaleMatx ::Num a=> a->[[a]]->[[a]]
scaleMatx a [] = []
scaleMatx a (r:rs) = map (* a) r:(scaleMatx a rs)  

nxndet ::Num a=> [[a]]->a
nxndet (x:xs) = 
 sum[(rsig [0..length x-1] q)*
      product[(x:xs)!!i!!(q!!i)|i<-[0..length x-1]]  |q<-(perms[0..length x-1])] 

randtrix ::(Num a,Random a,Integral b)=> Int->b->[[a]]
randtrix seed n = star seed n 0 []
 where star s n t ys
        | t==n = ys		---Random nxn matrix
        | otherwise = [ r|r<-(take (fromIntegral n)(randomRs (-200,200) (mkBlanket s)))]
		:star ((fst.next.mkBlanket) s) n (t+1) ys

idMatx ::(Eq a,Num a)=>Int-> [[a]]
idMatx 0 = []
idMatx n = (remdups.perms) (1:zeros (n-1))


-------Helpers
--cls = putStr "\ESC[2J"
fallout [] = []
fallout (p:ps) = p++fallout ps
chop :: Int->[a]->[[a]]
chop n [] = []
chop n ts = take n ts:chop n (drop n ts)
realtopair :: R->(Z,Z)
realtopair t = re (floor(t*(10^18)), floor (10^18) )
blur :: [[R]]->[[R]]
blur [] = []
blur (x:xs) = (map mkzero x):blur xs
  where
	mkzero r| r< (abs 0.0001) = 0.0
		| otherwise = r
-- re :: (Z,Z) -> (Z,Z) --reduces two fractions.
-- re (a,b) = (a `div` (gcd a b), b `div` (gcd a b))
swap :: [a]->Int->[a]
swap xs n = take n xs++[xs!!(n+1)]++[xs!!n]++drop (n+2) xs
zeros :: Num a=>Int->[a]
zeros 0 = []
zeros n = 0:(zeros (n-1))
remdups :: Eq a => [a]->[a]  
remdups [] = []           
remdups (x:xs) = x:remdups (filter (/= x) xs)
perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map(interleave x) (perms xs))
  where interleave x [] = [[x]]
        interleave x (y:ys) = (x:y:ys):map (y:) (interleave x ys)
	--Probabilistic Permutation Signature
rsgn ::Ord a=> [a]->[a]->String
rsgn xs ys = p xs 0 (mkBlanket 42)
   where 
    p list t r
      | length list /= length ys = "Lists have different sizes!" 
      | xs /= qsort list = "not equivalent lists!"
      | list == ys = if t`mod`2==0 then "( 1)" else "(-1)"
      | otherwise =  p (swap list (((`mod`(length xs-1)).fst.next) r))(t+1)((snd.next) r)
rsig ::(Num b,Ord a)=> [a]->[a]->b
rsig xs ys = p xs 0 (mkBlanket 42)
   where 
    p list t r
      | list == ys = if t`mod`2==0 then 1 else (-1)
      | otherwise =  p (swap list (((`mod`(length xs-1)).fst.next) r))(t+1)((snd.next) r)
	----------


