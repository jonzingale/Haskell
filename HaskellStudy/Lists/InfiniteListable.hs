module InfiniteListable where
import Listable

integers = Z (map (\x -> -x) [1..]) 0 [1..]

data Zipper a = Z [a] a [a] deriving Eq

instance Show a => Show (Zipper a) where
  show (Z as b cs) = unwords.map show $ [(reverse.take 10) as, [b], take 10 cs]

class Shiftable z where
  right, left :: z a -> z a
  focus :: z a -> a

instance Shiftable Zipper where
  right (Z as b (c:cs)) = Z (b:as) c cs
  left  (Z (a:as) b cs) = Z as a (b:cs)
  focus (Z _ b _) = b

instance Functor Zipper where
  fmap f (Z as b cs) = Z (map f as) (f b) (map f cs)

-- instance Applicative Zipper where
 -- not really sure what,
 --thought it could simplify cons and +++

{--
These should in some way be focus-centric. 
Perhaps, take a lead from:
import Data.Ratio
-- enumerate all rational numbers
fix ((1%1 :) . (>>= \x -> [x+1, 1/(x+1)]))
  [1%1,2%1,1%2,3%1,1%3,3%2,2%3,4%1,1%4,4%3,...]
--}
instance (Eq a, Listable a) => Listable (Zipper a) where
  takeL n = fmap $ takeL n
  dropL n = fmap $ dropL n
  unit = Z [] unit []
  -- negative numbers mess this stuff up.
  cons (Z as b cs) (Z xs y zs) = Z (f as xs) (cons b y) (f cs zs)
    where f a b = zipWith cons (headL a) b

  (+++) (Z as b cs) (Z xs y zs) = Z (f as xs) (b +++ y) (f cs zs)
    where f a b = zipWith (+++) a b