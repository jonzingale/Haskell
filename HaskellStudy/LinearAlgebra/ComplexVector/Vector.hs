{-# OPTIONS_GHC -Wno-missing-methods #-} -- because of signum, fromInteger, *

module Vector ((+), (-), (<|>), conj, eval, norm, abs, randVect,
                        ThreeVector(V3, S), Vector, Vect(Vect, getVect)) where
import System.Random
import Complex

cv = V3 (C 1 (-1)) (C 2 3) (C 5 0)
rv = V3 2.0 3.0 (-5.0)

data ThreeVector a = V3 a a a | S a deriving (Eq, Show)

newtype Vect a = Vect { getVect :: ThreeVector a }

instance Functor Vect where
  fmap f (Vect (V3 x y z)) = Vect $ V3 (f x) (f y) (f z)

wrap :: (a -> a) -> ThreeVector a -> ThreeVector a
wrap f vect = getVect.fmap f $ Vect vect

instance Comp a => Comp (ThreeVector a) where
  conj = wrap conj

class Vector v where
  (<|>) :: v -> v -> v
  eval :: v -> v
  norm :: v -> v

instance (Floating a, Num a, Comp a) => Vector (ThreeVector a) where
  (<|>) (V3 a b c) (V3 x y z) = V3 (conj a *x) (conj b *y) (conj c*z) -- Hermitian
  eval (V3 a b c) = S $ a + b + c
  norm = eval.abs

instance (Floating a, Num a, Comp a) => Num (ThreeVector a) where
  (+) (V3 a b c) (V3 x y z) = V3 (a+x) (b+y) (c+z)
  (-) (V3 a b c) (V3 x y z) = V3 (a-x) (b-y) (c-z)
  (*) (V3 a b c) (S x) = V3 (a*x) (b*x) (x*x)
  (*) (S x) (V3 a b c) = V3 (a*x) (b*x) (x*x)
  abs vect = wrap sqrt (vect <|> vect)

randVect :: ThreeVector Complex
randVect = let seed = mkStdGen 3 in
           let [a, b, c, d, e, f] = take 6 $ randomRs (0, 10) seed in
           V3 (C a d) (C b e) (C c f)