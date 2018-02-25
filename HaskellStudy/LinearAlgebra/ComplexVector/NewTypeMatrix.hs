{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-- {-# OPTIONS_GHC -Wno-missing-methods #-} -- because of signum, fromInteger

module ComplexMatrix where
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

newtype Matx a = Matx { getMatx :: ThreeMatrix (ThreeVector a)}

-- Maybe extend ThreeMatrix to Vector
-- eval, norm, <|>, projections.
instance Vector ThreeMatrix where
  projections (M3 a b c) = [a, b, c]

instance Comp a => Comp (ThreeMatrix a) where
  conj = fmap conj

class Matrix m where
  transpose :: m a -> m a
  -- det :: (Num a, Floating a, Comp a) => m a -> a 
  -- diag :: ThreeVector a -> m a
  -- unit :: a -> m a

instance Matrix Matx where
   transpose (Matx (M3 x y z)) =
      let [a, b, c] = projections x in
      let [d, e, f] = projections y in
      let [g, h, i] = projections z in
      Matx $ M3 (V3 a d g) (V3 b e h) (V3 c f i)

-- instance (Floating a, Num a, Comp a) => Num (Mtrx a) where
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

{--
DEVELOPED FROM PYTHON EXAMPLE
import numpy as np

def power_iteration(A, num_simulations):
    # Ideally choose a random vector
    # To decrease the chance that our vector
    # Is orthogonal to the eigenvector
    b_k = np.random.rand(A.shape[0])

    for _ in range(num_simulations):
        # calculate the matrix-by-vector product Ab
        b_k1 = np.dot(A, b_k)

        # calculate the norm
        b_k1_norm = np.linalg.norm(b_k1)

        # re normalize the vector
        b_k = b_k1 / b_k1_norm

    return b_k

power_iteration(np.array([[0.5, 0.5], [0.2, 0.8]]), 10)
--}