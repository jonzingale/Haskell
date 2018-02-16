module ComplexVector where
import Complex

data Vector x = V3 x x x deriving (Show, Eq)

cv1 = V3 c1 (c1*c1) (c1*3)

instance Functor Vector where
  fmap f (V3 x y z) = V3 (f x) (f y) (f z)

instance Applicative Vector where
  (V3 f g h) <*> (V3 x y z) = V3 (f x) (g y) (h z)
  pure t = V3 t t t

instance Num v => Num (Vector v)  where
  fromInteger x = fmap fromInteger $ V3 x x x
  (+) v w = (+) <$> v <*> w
  (*) v w = (*) <$> v <*> w -- verify this.
  negate v = (-1) * v
  abs v = v * v
  signum = id -- arbitrary