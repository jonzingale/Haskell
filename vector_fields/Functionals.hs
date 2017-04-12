module Functionals where

{--
  Linear Functionals are here defined by a finite list
  of inputs, ie a function R^k -> R for some number k
  and ring R. Functionals R^2 -> R, with 2 < k for instance,
  is treated as an inclusion of R^k -> R where all 2 < i < k
  are zero. For typographical simplicity, k = 3.
--}

data Functional a = C a | Lf (a -> a -> a -> a) 

eval :: Num a => (Functional a) -> a -> a -> a -> a
eval (Lf f) = f

f3 = Lf $ \x y z -> (10 * x)^2 + 2*y + z
test = eval (3 * (f3 + f3)) 5 2 1

-- ‘abs’, ‘signum’
instance (Num a) => Num (Functional a) where
  (+) (Lf f) (Lf g) = Lf $ \x y z -> f x y z + g x y z
  (-) (Lf f) (Lf g) = Lf $ \x y z -> f x y z - g x y z
  (*) (Lf f) (Lf g) = Lf $ \x y z -> f x y z * g x y z
  (*) (C a)  (Lf f) = Lf $ \x y z -> a * f x y z
  fromInteger = C . fromIntegral
  negate f = C (-1) * f