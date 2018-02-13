module VectorFunctorial where

data ThreeVect a = V a a a deriving (Show, Eq)

instance Functor ThreeVect where
  fmap f (V a b c) = V (f a) (f b) (f c)

vs, ws :: Floating v => ThreeVect v
vs = V 3 (-3) 1
ws = V 4 9 2

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
