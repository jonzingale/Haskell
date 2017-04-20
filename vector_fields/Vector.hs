module Vector where

data Vector x = S x | V3 x x x deriving Show

instance Functor Vector where
  fmap f (V3 x y z) = V3 (f x) (f y) (f z)
  fmap f (S x) = S $ f x

instance Applicative Vector where
  (V3 f g h) <*> (V3 x y z) = V3 (f x) (g y) (h z)
  (S f) <*> (V3 x y z) = V3 (f x) (f y) (f z)
  (S f) <*> (S x) = S $ f x
  pure t = S t

instance Num v => Num (Vector v)  where
  fromInteger x = S $ fromInteger x
  (+) v w = (+) <$> v <*> w
  (*) v w = (*) <$> v <*> w
  abs v = v * v
  negate v = S (-1) * v
  signum x = S 1 -- figure this out

-- Below: This is nice for general Transformations,
-- but will not work as nicely for Functionals
-- defined nearby.

-- class Num v => Linear v where
--   (<|>) :: v -> v -> v
--   eval :: v -> v

-- instance Num v => Linear (Vector v) where
--   (<|>) (V3 a b c) (V3 x y z) = V3 (a * x) (b * y) (c * z)
--   (<|>) (V2 a b) (V2 x y) = V2 (a * x) (b * y)
--   (<|>) (S a) (S x) = S $ a * x

--   eval (V3 x y z) = S (x + y + z)
--   eval (V2 x y) = S (x + y)
--   eval (S a) = S a
