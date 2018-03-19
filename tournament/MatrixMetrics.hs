module MatrixMetrics (spectra, upperTri, adjacency) where
import qualified Numeric.LinearAlgebra.HMatrix as H
import Helpers
import Graphs

{--
This is a module for computing matrix metrics 
such as the spectrum of a graph.

H.rank, H.orth, H.outer, H.det, H.ident
--}

type DMatrix = H.Matrix Double
type HComplex = H.Complex Double

tetraA, cycle3 :: DMatrix
tetraA = adjacency.take 4 $ repeat 3
cycle3 = (H.><) 3 3 [0,1,0, 0,0,1, 1,0,0]
simplexA n = adjacency.take (n+1) $ repeat n  
exampleA = adjacency [5,4,3,4,2,2]

-- spectra from tetra: [(-1,3),(3,1)]
spectra :: (Eq a, Integral a) => DMatrix -> [(a, Int)]
spectra matrix = collect.rounded_reals.eigenlist $ matrix
  where
    eigenlist = (H.toList).(H.eigenvalues)
    rounded_reals = map (round.H.realPart)
    collect [] = []
    collect (a:[]) = [(a, 1)]
    collect (a:as) = (a, length.filter (== a) $ (a:as)) :
                      (collect $ filter (/= a) as)

-- Just without a Nothing.
upperTri :: [Int] -> DMatrix
upperTri degrees = 
  let n = length degrees in
  let (Just array) = hhAdjacency degrees in
  (H.><) n n (map fromIntegral array)

adjacency :: [Int] -> DMatrix
adjacency = \ds -> upperTri ds + H.tr (upperTri ds)
