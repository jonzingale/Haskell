module VectorFunctorial where

data ThreeVect a = V a a a deriving (Show, Eq)

instance Functor ThreeVect where
  fmap f (V a b c) = V (f a) (f b) (f c)

vs = V 3.0 (-3.0) 1.0
ws = V 4.0 9.0 2.0

class Vector w where
  (%) :: w -> w -> w -- cross product

instance Num v => Vector (ThreeVect v) where
  (%) (V a b c) (V x y z) = V (b*z-y*c) (c*x-a*z) (a*y-b*x)

instance Floating v => Num (ThreeVect v) where
  fromInteger x = fromInteger <$> V x x x -- fascilitates scalar multiplication
  (+) (V a b c) (V x y z) = V (a+x) (b+y) (c+z)
  (*) (V a b c) (V x y z) = V (a*x) (b*y) (c*z) -- dot product
  (-) (V a b c) (V x y z) = V (a-x) (b-y) (c-z)
  abs vect = sqrt <$> vect * vect
  signum vect = signum <$> vect
