module MatrixMetrics where
import qualified Numeric.LinearAlgebra.HMatrix as H
-- can i import unqualified?

{--
This is a module for computing matrix metrics 
such as the spectrum of a graph.

Todo: more sophisticated rounding: spectra cycle3
--}

type DMatrix = H.Matrix Double
type HComplex = H.Complex Double

t1 = [(8,7),(8,6),(8,5),(4,3),(4,2),(4,1),(7,6),(7,5),(3,2),(3,1),(6,5),(2,1)]

-- graphToMatrix :: [Int,Int] -> DMatrix


tetra :: DMatrix
tetra = (H.><) 4 4 [0,1,1,1,1,0,1,1,1,1,0,1,1,1,1,0]

cycle3 :: DMatrix
cycle3 = (H.><) 3 3 [0,1,0, 0,0,1, 1,0,0]

-- spectra from tetra: [(-1,3),(3,1)]
spectra :: (Eq a, Integral a) => DMatrix -> [(a, Int)]
spectra matrix = collect.rounded_reals.eigenlist $ matrix
  where
    eigenlist = (H.toList) . (H.eigenvalues)
    rounded_reals = map (round.H.realPart)
    collect [] = []
    collect (a:[]) = [(a, 1)]
    collect (a:as) = (a, length.filter (== a) $ (a:as)) :
                      (collect $ filter (/= a) as)

-- builds a diagonal matrix based on the degrees of vertices.
-- del :: DMatrix -> DMatrix
-- del matrix = 

