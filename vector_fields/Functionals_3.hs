module Functionals3 where

{--
  d
M -> N   given d & f: pullback
f\ /g    given f & d: invertible d (section)
  r      give f & g: linear map (coalgebra?)
--}

-- Linear Algebra as recursive function type
data RLinear a = Zero | Lf (a -> a) | TT (RLinear a) (RLinear a) | C a

pr1 (TT a b) = a
pr2 (TT a b) = b

f0 = Lf $ \x -> 3 * x
f1 = Lf $ \x -> 2 * x + 1

ff = TT f0 f1

eval :: Num a => RLinear a -> a -> a
eval (TT f g) = eval (f + g)
eval Zero = (* 0)
eval (Lf f) = f

-- ‘abs’, ‘signum’
instance (Num a) => Num (RLinear a) where
  (+) (Lf f) (Lf g) = Lf $ \x -> f x + g x
  (+) (TT f g) h = f + g + h
  (+) h (TT f g) = h + f + g
  (+) Zero f = f
  (+) f Zero = f

  (-) (Lf f) (Lf g) = Lf $ \x -> f x - g x
  (-) (TT f g) h = f - g - h
  (-) h (TT f g) = h - f - g
  (-) Zero f = f
  (-) f Zero = f

  (*) (C a) (Lf f) = Lf $ (\g x -> g (a * x)) f
  (*) (C a) (TT f g) = TT ((C a) * f) ((C a) * g)
  (*) (Lf f) (Lf g) = Lf $ \x -> f x * g x
  (*) (TT f g) h = f * g * h
  (*) h (TT f g) = h * f * g

  fromInteger = C . fromIntegral
  negate f = C (-1) * f