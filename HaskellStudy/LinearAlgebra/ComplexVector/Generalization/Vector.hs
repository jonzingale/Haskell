{-# OPTIONS_GHC -Wno-missing-methods #-} -- because of signum, fromInteger, *

module Vector where
import Complex

cv = CV (C 1 (-1)) (C 2 3) (C 5 0)
dd = CV (C 0 0) (C 1 (-1)) (C 0 0)
rv = RV 2 3 (-5)

data RealVector = RV Double Double Double deriving (Eq, Show)
data CompVector = CV Complex Complex Complex deriving (Eq, Show)

class Vector v where
  coord :: v -> (Complex, Complex, Complex)
  (<|>) :: v -> v -> v
  eval :: v -> Complex
  norm :: v -> Complex
  vconj :: v -> v

instance Vector RealVector where
  coord (RV a b c) = (incl a, incl b, incl c)
  (<|>) (RV a b c) (RV x y z) = RV (a*x) (b*y) (c*z) 
  eval (RV a b c) = incl $ a + b + c
  norm = eval.abs
  vconj = id

instance Vector CompVector where
  coord (CV a b c) = (a, b, c)
  (<|>) (CV a b c) (CV x y z) = CV (conj a *x) (conj b *y) (conj c*z) -- Hermitian
  vconj (CV a b c) = CV (conj a) (conj b) (conj c)
  eval (CV a b c) = a + b + c
  norm = eval.abs

instance Num RealVector where
  (+) (RV a b c) (RV x y z) = RV (a+x) (b+y) (c+z)
  (-) (RV a b c) (RV x y z) = RV (a-x) (b-y) (c-z)
  abs vect = let (RV a b c) = vect <|> vect in
    RV (sqrt a) (sqrt b) (sqrt c)

instance Num CompVector where
  (+) (CV a b c) (CV x y z) = CV (a+x) (b+y) (c+z)
  (-) (CV a b c) (CV x y z) = CV (a-x) (b-y) (c-z)
  abs vect = let (CV a b c) = vect <|> vect in
    CV (sqrt a) (sqrt b) (sqrt c)
