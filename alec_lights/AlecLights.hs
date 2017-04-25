module AlecLights where
import Numeric.Matrix hiding (map)
import Data.Bool

rotate :: [a] ->[[a]]
rotate xs =  xs : rotate (last xs : take (length xs - 1) xs)

n_many :: Int -> a -> [a]
n_many n a = take n $ repeat a

squareM :: Int -> Matrix Float
squareM n = let mid_zeros = n_many (n-2) 0 in
            let first = ((n_many 2 id) <*> [1.0]) ++ mid_zeros  in
            let last = mid_zeros ++ n_many 2 1.0 in
            let rows = n_many 3 1.0 ++ n_many (n-3) 0  in
            let middles = take (n-2) $ rotate rows in
            fromList $ first : middles ++ [last]

square :: Int -> [[Int]]
square n = let mid_zeros = n_many (n-2) 0 in
           let first = ((n_many 2 id) <*> [1]) ++ mid_zeros  in
           let last = mid_zeros ++ n_many 2 1 in
           let rows = n_many 3 1 ++ n_many (n-3) 0  in
           let middles = take (n-2) $ rotate rows in
           first : middles ++ [last]

singulars = [(i, mod i 3) | i<-[3..], (det.squareM) i == 0 ]

toLights xs = (map L) xs

---------

big4 = [[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]]

(<+>) :: Integral a => [a] -> [a] -> [a]
(<+>) as bs = [(a + b) `mod` 2 | (a, b)<- zip as bs]

reduce :: Integral a => [[a]] -> [[a]]
reduce (a:b:cs) | Prelude.all (== 0) a = reduce (b:cs)
                | Prelude.all (== 1) $ map sum (a:b:cs) = (a:b:cs)
                | otherwise = reduce $ (b:cs) ++ [rr a (b:cs)]
  where
    rr x [] = x
    rr x (y:ys) | sum x > 1 = rr (x <+> y) ys
                | otherwise = x

----
{--
Rank=m; //m is number of your rows

for(i=0;i<NumberofRows<i++)
{
    Row[i]=Row[i] XOR Row[i+1]
   for(j=0;j<NumberofColumns;j++)
  {
    if element(j)==1 continue; //element is an individual in the row
   else rank--;
  }
}
--}
----
data Lights a = L [a] deriving (Show, Eq)

l1 = L [1,0,1,0,0,1,0,0]

instance Functor Lights where
  fmap f (L as) = L $ map f as

instance (Integral a, Num a) => Monoid (Lights a) where
  mempty = L []
  mappend = (+)

instance Applicative Lights where
  (L fs) <*> (L as) = L [ f a | (f, a) <- zip fs as]
  pure x = L $ repeat x

instance (Num a, Integral a) => Num (Lights a) where
  fromInteger x = L $ map fromInteger $ repeat x
  (+) v w = (`mod` 2) <$> ((+) <$> v <*> w)
  (-) v w = (`mod` 2) <$> ((-) <$> v <*> w)
  (*) v w = (`mod` 2) <$> ((*) <$> v <*> w)
  signum (L as) = L [1]
  abs = id