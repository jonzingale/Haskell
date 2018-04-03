module Attenuation where
import InterceptMethod
import Data.Array
{--
TODO:
Make a 1000 cubic-cell mesh and calculate the path sums through
the mesh with a function at every point. The value of the function
gives the amount of attenuation through the mesh.

Further, the length of the ray through each given
cell is needed to compute the value at the given cell.
--}



-- Given position (x,y) and incidence (θ,φ)
-- return a list of indices through the mesh.
