
{-# LANGUAGE MultiParamTypeClasses #-} -- for matrix dependent on vector
{-# LANGUAGE FunctionalDependencies #-} -- for matrix dependent on vector
{-# OPTIONS_GHC -Wno-missing-methods #-} -- for Num and Vector ThreeMatrix

module Matrix where
import Complex
import Vector
import Bit

cv = V3 (C 1 (-1)) (C 2 3) (C 5 0)
rv = V3 2.0 3.0 (-5.0)

data ThreeMatrix a = M3 a a a deriving Eq

instance Functor ThreeMatrix where
  fmap f (M3 x y z) = M3 (f x) (f y) (f z)

instance Show a => Show (ThreeMatrix a) where
  show (M3 a b c) = (unlines.map show) [a, b, c]

-- Maybe extend ThreeMatrix to Vector
-- eval, norm, <|>,
instance Vector ThreeMatrix where
  prs (M3 a b c) = [a, b, c]
  -- (<|>) (M3 a b c) (M3 x y z) = https://en.wikipedia.org/wiki/Frobenius_inner_product -- <A*,B>
  -- (<|>) (M3 a b c) (V3 x y z) = fmap (<|> (incl (V3 x y z))) (M3 a b c) -- bad typing

instance Comp a => Comp (ThreeMatrix a) where
  conj = fmap conj

class Matrix m v | v -> m where
  tr :: Comp v => m v -> m v -- Hermitian transpose
  incl :: v -> m v

instance (Num a) => Matrix ThreeMatrix (ThreeVector a) where
  tr m = let [a, b, c, d, e, f, g, h, i] = foldr (++) [] $ prs.fmap prs $ m in
    conj $ M3 (V3 a d g) (V3 b e h) (V3 c f i)

  incl (V3 a b c) = M3 (V3 a 0 0) (V3 0 b 0) (V3 0 c 0)

-- instance (Floating a, Num a, Comp a) => Num (ThreeMatrix a) where
  -- (+) (M a b c) (M x y z) = M (a+x) (b+y) (c+z)
  -- (-) (M a b c) (M x y z) = M (a-x) (b-y) (c-z)
  -- (*) (CM a b c) (CM x y z) = -- (AB)* = (A*)(B*)

mm :: ThreeMatrix (ThreeVector Complex)
mm = M3 (V3 (C 1 2) (C 2 3) (C 3 4))
        (V3 (C 4 2) (C 5 3) (C 6 4))
        (V3 (C 7 2) (C 8 3) (C 8 4))

bb :: ThreeMatrix (ThreeVector Bit)
bb = M3 (V3 Zero One Zero)
        (V3 One Zero One)
        (V3 One One Zero)

rr :: ThreeMatrix (ThreeVector Double)
rr = M3 (V3 1 2 3)
        (V3 4 5 6)
        (V3 7 8 8)
