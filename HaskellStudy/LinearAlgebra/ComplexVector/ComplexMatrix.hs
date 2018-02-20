{-# LANGUAGE OverloadedStrings #-} -- for Data.Text.Format

module ComplexMatrix where
import ComplexVector
import Complex

import System.Random

-- towards eigenvectors via poweriteration.
poweriteration :: Matrix Complex -> Vector Complex
poweriteration mat =
  let b_k = randVect in recursivef mat b_k 10
    where
      recursivef _ evect 0 = evect
      recursivef m vect n = 
          let b_k1 = multip m vect in
          let b_k1_norm = abs b_k1 in
          -- let bk = b_k1 / b_k1_norm in -- <-- division how?
          let bk = b_k1 * b_k1_norm in -- divison as multiplication by conjugate

          recursivef mat bk (n-1)


data Matrix c = M (Vector c) (Vector c) (Vector c) deriving Eq

instance Show c => Show (Matrix c) where
  show (M a b c) = (unlines.map show) [a, b, c]

randVect :: Vector Complex
randVect = let seed = mkStdGen 3 in
           let [a, b, c, d, e, f] = take 6 $ randomRs (0, 10) seed in
           V3 (C a d) (C b e) (C c f)

mm :: Matrix Complex
mm = M cv1 dd cv1

rr :: Matrix Double
rr = M (V3 1 2 3) (V3 4 5 6) (V3 7 8 8)

multip :: (Num c) => Matrix c -> Vector c -> Vector c -- simplify later by inheritance
multip (M (V3 a b c) (V3 d e f) (V3 g h i)) (V3 x y z) =
  V3 (a*x + b*y + c*z) (d*x + e*y + f*z) (g*x + h*y + i*z)


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