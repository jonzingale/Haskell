module Attenuation where
import InterceptMethod
import Data.Array
{--
TODO:
Make a 1000 cubic-cell mesh and calculate the path sums through
the mesh with a function at every point. The value of the function
gives the amount of attenuation through the mesh.
--}



-- Given position (x,y) and incidence (θ,φ)
-- return a list of indices through the mesh.

type Slope = (Integer, Integer) -- may need to rethink these types.
type Indices = [(Integer, Integer)]


rhythm :: Slope -> Indices
rhythm (n, m) | (abs n) > m = g (1+ div (abs n) m) (0,0) (signum n)
              | otherwise = swaps $ g (1+ div m (abs n)) (0,0) (signum n)
  where
    swaps = map (\(x,y)->(y,x))

    g n (i, j) sig = 
      let ary = [(i, j+p) | p <- [0..]] in
      take (fromIntegral n) ary ++ g n (sig*(i+1), j+n-1) sig

test1 = take 10 (rhythm (5,2)) == [(0,0),(0,1),(0,2),(1,2),(1,3),(1,4),(2,4),(2,5),(2,6),(3,6)]
test2 = take 6 (rhythm (-5,2)) == [(0,0),(0,1),(0,2),(-1,2),(-1,3),(-1,4)]

tests = test1 && test2