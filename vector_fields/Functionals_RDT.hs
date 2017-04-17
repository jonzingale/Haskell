module Functionals3 where

{--
  d
M -> N   given d & f: pullback
f\ /g    given f & d: invertible d (section)
  r      give f & g: linear map (coalgebra?)
--}

-- Linear Algebra as Recursive Function Type
data RLinear a = Zero | C a | Lf (a -> a) | TT (RLinear a) (RLinear a) | Pt [a]

incl :: Num a => [a] -> RLinear a
incl xs = Pt $ xs ++ repeat 0

eta :: Num a => a -> RLinear a
eta n = Pt $ repeat n

-- how do i pass arguments in a natural way?
eval2 (Lf f) (Pt (x:xs)) = f x -- ignores extra inputs.
eval2 (TT (Lf f) (Lf g)) (Pt (x:y:xs)) = f x + g y -- not the same as (Lf f + Lf g) (Pt xs)!?
eval2 (TT (Lf f) (TT g h)) (Pt (x:xs)) = f x + eval2 (TT g h) (Pt xs)
eval2 (TT (TT f g) (Lf h)) (Pt p) = eval2 (TT f $ TT g (Lf h)) $ Pt p

-- eval2 (TT (TT f g) (TT i j)) (Pt p) = No idea.
-- eval2 fsix (incl [1..6])

pr1 (TT a b) = a
pr2 (TT a b) = b

f0 = Lf $ \x -> 3 * x
f1 = Lf $ \x -> 2 * x + 1

ff = TT f0 f1
fgg = TT ff f1
ffg = TT f0 ff
fsix = TT ffg fgg

-- -- this isn't quite correct as eval ff 2
-- -- ought not have meaning, should it?
-- eval :: Num a => RLinear a -> a -> a
-- eval (TT f g) = eval (f + g)
-- eval (C a) = (* a)
-- eval Zero = (* 0)
-- eval (Lf f) = f

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