module Functionals where

{--
  Linear Functionals are here defined by a finite list
  of inputs, ie a function R^k -> R for some number k
  and ring R. Functionals R^2 -> R, with 2 < k for instance,
  is treated as an inclusion of R^k -> R where all 2 < i < k
  are zero. For typographical simplicity, k = 3.
--}

f1, f2, f3 :: Functional Integer
f1 = Lf $ \x y z -> x + 10 * y + 100 * z
f2 = Lf $ \x y z -> x + x * y + 2 * z
f3 = Lf $ \x y z -> x * z + x * y + z^2

test1 = eval (3 * (f3 + f2)) 5 2 1
test2 f = eval1 (pr1 f) 1 + eval1 (pr2 f) 2 + eval1 (pr3 f) 3 == eval f 1 2 3
-- test2 is only generally true when linearly independent

data Functional a = C a | Lf (a -> a -> a -> a) | Pi (a -> a)

eval :: Num a => (Functional a) -> a -> a -> a -> a
eval (Lf f) = f

eval1 :: Num a => (Functional a) -> a -> a
eval1 (Pi f) = f

pr1, pr2, pr3 :: Num a => Functional a -> Functional a
pr1 (Lf f) = Pi $ (\f i -> f i 0 0) f
pr2 (Lf f) = Pi $ (\f i -> f 0 i 0) f
pr3 (Lf f) = Pi $ (\f i -> f 0 0 i) f

-- ‘abs’, ‘signum’
instance (Num a) => Num (Functional a) where
  (+) (Lf f) (Lf g) = Lf $ \x y z -> f x y z + g x y z
  (-) (Lf f) (Lf g) = Lf $ \x y z -> f x y z - g x y z
  (*) (Lf f) (Lf g) = Lf $ \x y z -> f x y z * g x y z
  (*) (C a)  (Lf f) = Lf $ \x y z -> a * f x y z
  fromInteger = C . fromIntegral
  negate f = C (-1) * f