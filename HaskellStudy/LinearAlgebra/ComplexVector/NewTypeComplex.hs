{-# OPTIONS_GHC -Wno-missing-methods #-} -- because of signum, fromInteger, *

module NewTypeComplex where
import Complex

cv = V3 (C 1 (-1)) (C 2 3) (C 5 0)
rv = V3 2.0 3.0 (-5.0)

data ThreeVector a = V3 a a a | S a deriving (Eq, Show)

newtype Vect a = Vect { getVect :: ThreeVector a }

instance Functor Vect where
  fmap f (Vect (V3 x y z)) = Vect $ V3 (f x) (f y) (f z)

wrap :: (a -> a) -> ThreeVector a -> ThreeVector a
wrap f vect = getVect.fmap f $ Vect vect

class Vector v where
  (<|>) :: v -> v -> v
  vconj :: v -> v
  eval :: v -> v
  norm :: v -> v

instance (Floating a, Num a, Comp a) => Vector (ThreeVector a) where
  (<|>) (V3 a b c) (V3 x y z) = V3 (conj a *x) (conj b *y) (conj c*z) -- Hermitian
  vconj = wrap conj -- perhaps extend Comp
  eval (V3 a b c) = S $ a + b + c
  norm = eval.abs

instance (Floating a, Num a, Comp a) => Num (ThreeVector a) where
  (+) (V3 a b c) (V3 x y z) = V3 (a+x) (b+y) (c+z)
  (-) (V3 a b c) (V3 x y z) = V3 (a-x) (b-y) (c-z)
  abs vect = wrap sqrt (vect <|> vect)