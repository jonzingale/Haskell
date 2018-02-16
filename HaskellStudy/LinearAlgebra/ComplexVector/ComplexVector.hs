module ComplexVector where
import Complex

data Vector x = S x | V2 x x | V3 x x x deriving (Show, Eq)

cv3 = V3 c1 (c1*c1) (c1*3)
cv2 = V2 (C 1 2) (C 3 (-1))
cv22 = V2 (C (-2) 1) (C 4 0)

instance Functor Vector where
  fmap f (V3 x y z) = V3 (f x) (f y) (f z)
  fmap f (V2 x y) = V2 (f x) (f y)
  fmap f (S x) = S (f x)

instance Applicative Vector where
  (V3 f g h) <*> (V3 x y z) = V3 (f x) (g y) (h z)
  (V2 f g) <*> (V2 x y) = V2 (f x) (g y)
  pure t = S t

instance Num v => Num (Vector v)  where
  fromInteger x = fmap fromInteger $ V3 x x x
  (+) v w = (+) <$> v <*> w
  (*) v w = (*) <$> v <*> w-- this ought to be innerproduct. conj ect . . ., but we don't know it's complex!
  negate v = (-1) * v
  abs v = v * v --verify
  signum = id -- arbitrary