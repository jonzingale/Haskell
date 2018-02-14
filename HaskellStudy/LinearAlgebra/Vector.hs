module Vector where

data ThreeVect = V Double Double Double deriving (Show, Eq)

eval (V a b c) = a + b + c

-- because I don't have Functor ThreeVect.
vmap f (V a b c) = V (f a) (f b) (f c)

vs = V 3.0 (-3.0) 1.0
ws = V 4.0 9.0 2.0

class Vector w where
  (%) :: w -> w -> w -- cross product

instance Vector ThreeVect where
  (%) (V a b c) (V x y z) = V (b*z-y*c) (c*x-a*z) (a*y-b*x)

instance Num ThreeVect where
  fromInteger x = let xx = fromInteger x in V xx xx xx
  -- fascilitates scalar multiplication

  (+) (V a b c) (V x y z) = V (a+x) (b+y) (c+z)
  (*) (V a b c) (V x y z) = V (a*x) (b*y) (c*z) -- dot product
  (-) (V a b c) (V x y z) = V (a-x) (b-y) (c-z)
  abs vect = vmap sqrt $ vect * vect
  signum vect = vmap signum $ vect
