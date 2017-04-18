module Functionals3 where

{--
  d
M -> N   given d & f: pullback
f\ /g    given f & d: invertible d (section)
  r      give f & g: linear map (coalgebra?)
--}

{--NOTE:
Operations are defined such that the rank
of the left argument takes precedence.
--}

-- Linear Algebra as Array
data Linear a = Zero | C a | Lf (a -> a) | TT [Linear a] | Pt [a]

--- Arguments
incl :: Num a => [a] -> Linear a
incl xs = Pt $ xs ++ repeat 0

eta :: Num a => a -> Linear a
eta n = Pt $ repeat n

nthF :: Int -> Linear a
nthF n = TT $ take (n-1) (repeat Zero) ++ [Lf id]

--- Universals
eval :: Num b => Linear b -> Linear b -> b
eval (TT fs) (Pt xs) = sum [eval f $ Pt [x] | (f, x) <- zip fs xs]
eval (Lf f) (Pt (x:xs)) = f x
eval (C a) (Pt t) = a
eval Zero (Pt t) = 0

prj :: Int -> Linear t -> Linear t
prj n (TT fs) = fs!!n
-- prj n (TT as) = compose (TT as) (nthF n)

--- Functions
pt = Pt [1..]
f0 = Lf $ \x -> 3 * x
f1 = Lf $ \x -> 2 * x + 1

ff = TT [f0, f1]
ffg = TT [f0, f0, f1]
fgg = TT [f0, f1, f1]
fsix = TT $ uG ffg ++ uG fgg

--- HigherOrder Functions
o_ :: Num a => Linear a -> Linear a -> Linear a
o_ (Lf f) (C a) = (C a) * (Lf f)
o_ (Lf f) (Lf g) = Lf (f.g)
o_ (TT fs) (TT gs) = TT [ o_ f g| (f, g) <- zip fs gs ]

-- underlying array helper
uG :: Linear t -> [Linear t]
uG (TT fs) = fs

-- ‘abs’, ‘signum’
instance (Num a) => Num (Linear a) where
  (+) (Lf f) (Lf g) = Lf $ \x -> f x + g x
  (+) (TT fs) (TT gs) = TT [f + g | (f, g) <- zip fs gs]
  (-) (TT fs) (TT gs) = TT [f - g | (f, g) <- zip fs gs]
  (-) (Lf f) (Lf g) = Lf $ \x -> f x - g x
  -- (*) is inner product
  (*) (TT fs) (TT gs) = TT [f * g | (f, g) <- zip fs gs]
  (*) (C a) (Lf f) = Lf $ (\g x -> g (a * x)) f
  (*) (C a)  (TT fs) = TT $ map ((*) (C a)) fs
  (*) (Lf f) (Lf g) = Lf $ \x -> f x * g x
  fromInteger = C . fromIntegral
  negate f = C (-1) * f