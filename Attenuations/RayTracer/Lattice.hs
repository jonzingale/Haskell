
-- https://hackage.haskell.org/package/array-0.5.2.0/docs/Data-Array.html

module RayTracer.Lattice where
import RayTracer.Rhythm
import System.Random

import qualified Data.Array.Accelerate as A
import qualified Data.ByteString.Lazy as L
import Data.Array

{--
TODO:
Make a 1000 cubic-cell mesh and calculate the path sums through
the mesh with a function at every point. The value of the function
gives the amount of attenuation through the mesh.

Further, the length of the ray through each given
cell is needed to compute the value at the given cell.

10^9 cells eventually.
--}

-- rArray takes seed values to produce random arrays.
bigArray = listArray bounds $ randomRs (0::Double, 10**3).mkStdGen $ 42
  where bounds = (0, 10^6-1)