module Functionals where

data Functional a = Lf (a -> a) | C a

eval :: Num a => (Functional a) -> a -> a
eval (Lf f) a = f a

c1 = 2
f1 = Lf $ \x -> (10 * x)^2
test = eval (3 * (f1 + f1)) 5

-- ‘abs’, ‘signum’
instance (Num a) => Num (Functional a) where
  (+) (Lf f) (Lf g) = Lf $ \x -> f x + g x
  (-) (Lf f) (Lf g) = Lf $ \x -> f x - g x
  (*) (Lf f) (Lf g) = Lf $ \x -> f x * g x
  (*) (C a)  (Lf f) = Lf $ \x -> a * f x
  fromInteger = C . fromIntegral
  -- negate f = -1 * f -- not sane.