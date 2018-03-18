module MatrixMetrics where
import qualified Numeric.LinearAlgebra.HMatrix as H
import Helpers
import Graphs

{--
This is a module for computing matrix metrics 
such as the spectrum of a graph.

Todo: more sophisticated rounding: spectra cycle3
--}

type DMatrix = H.Matrix Double
type HComplex = H.Complex Double

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

-- H.rank, H.orth, H.outer, H.det
upperTri :: [Int] -> DMatrix
upperTri degrees = 
  let n = length degrees in 
  let array = havelAdjacency degrees in
  (H.><) n n (map fromIntegral array)

symmetric :: [Int] -> DMatrix
symmetric = \ds -> upperTri ds + H.tr (upperTri ds)

adjacency :: [Double] -> DMatrix
adjacency array = let mat = (H.><) 3 3 array in
  mat + H.tr mat

--LOOK UP FORMAL VERIFICATION / PROVABLY CORRECT.
--Verify H.rank with Alec Lights.
