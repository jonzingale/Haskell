module RayTracer.Lattice where
import RayTracer.Rhythm
import System.Random
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

type Coords = Array Integer Double
data BigArray = VLA {pr1::Coords, pr2::Coords, pr3::Coords} deriving Show

-- rArray takes seed values to produce random arrays.
bigArray = VLA (rArray 21) (rArray 42) (rArray 23)
  where
    rArray = listArray (0,100).randomRs (0,10^3).mkStdGen