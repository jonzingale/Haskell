module ComplexMatrix where
import Complex
import Vector

import System.Random

data CompMatrix = CM CompVector CompVector CompVector deriving Eq
data RealMatrix = RM RealVector RealVector RealVector deriving Eq

instance Show CompMatrix where
  show (CM a b c) = (unlines.map show) [a, b, c]

instance Show RealMatrix where
  show (RM a b c) = (unlines.map show) [a, b, c]

randVect :: CompVector
randVect = let seed = mkStdGen 3 in
           let [a, b, c, d, e, f] = take 6 $ randomRs (0, 10) seed in
           CV (C a d) (C b e) (C c f)

mm :: CompMatrix
mm = CM cv dd cv

rr :: RealMatrix
rr = RM (RV 1 2 3) (RV 4 5 6) (RV 7 8 8)

-- likely needs functional dependencies for composition
class Matrix m where
  diag :: CompVector -> m
  multip :: m -> CompVector -> CompVector  -- conjugate how?

instance Matrix CompMatrix where
  diag (CV a b c) = CM (CV a 0 0) (CV 0 b 0) (CV 0 0 c)
  multip (CM (CV a b c) (CV d e f) (CV g h i)) (CV x y z) =
    CV (a*x + b*y + c*z) (d*x + e*y + f*z) (g*x + h*y + i*z)

-- instance Matrix RealMatrix where
  -- diag (RV a b c) = RM (RV a 0 0) (RV 0 b 0) (RV 0 0 c)
  -- multip (RM (RV a b c) (RV d e f) (RV g h i)) (RV x y z) =
    -- CV (a*x + b*y + c*z) (d*x + e*y + f*z) (g*x + h*y + i*z)

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