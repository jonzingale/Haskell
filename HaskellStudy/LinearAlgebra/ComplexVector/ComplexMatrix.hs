
{-# OPTIONS_GHC -Wno-missing-methods #-} -- because of signum, fromInteger

module ComplexMatrix where
import Complex
import Vector
import Bit

import System.Random

cv = V3 (C 1 (-1)) (C 2 3) (C 5 0)
rv = V3 2.0 3.0 (-5.0)

-- data ThreeMatrix a = M3 (ThreeVector a) (ThreeVector a) (ThreeVector a) deriving Eq
data ThreeMatrix a = M3 a a a deriving (Eq)

newtype Matrix a = Matrix { getMatrix :: ThreeMatrix a }

mwrap :: (a -> a) -> ThreeMatrix a -> ThreeMatrix a
mwrap f matx = getMatrix.fmap f $ Matrix matx

instance Functor Matrix where
  fmap f (Matrix (M3 x y z)) = Matrix $ M3 (f x) (f y) (f z)

instance Show a => Show (ThreeMatrix a) where
  show (M3 a b c) = (unlines.map show) [a, b, c]

instance Comp a => Comp (ThreeMatrix a) where
  conj (M3 a b c) = mwrap conj (M3 a b c) 

randVect :: ThreeVector Complex
randVect = let seed = mkStdGen 3 in
           let [a, b, c, d, e, f] = take 6 $ randomRs (0, 10) seed in
           V3 (C a d) (C b e) (C c f)

mm :: ThreeMatrix (ThreeVector Complex)
mm = M3 (V3 (C 1 2) (C 2 3) (C 3 4))
        (V3 (C 4 2) (C 5 3) (C 6 4))
        (V3 (C 7 2) (C 8 3) (C 8 4))

-- class MatrixOp m a where -- NEWTYPE TRICK?
--   diag :: ThreeVector a -> m a

-- instance Matrix CompMatrix where
--   diag (CV a b c) = CM (CV a 0 0) (CV 0 b 0) (CV 0 0 c)
--   multip (CM (CV a b c) (CV d e f) (CV g h i)) (CV x y z) =
--     CV (a*x + b*y + c*z) (d*x + e*y + f*z) (g*x + h*y + i*z)

-- instance (Floating a, Num a, Comp a) => Num (Matrix a) where
  -- (+) (M a b c) (M x y z) = M (a+x) (b+y) (c+z)
  -- (-) (M a b c) (M x y z) = M (a-x) (b-y) (c-z)
  -- (*) (CM a b c) (CM x y z) = -- (AB)* = (A*)(B*)


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