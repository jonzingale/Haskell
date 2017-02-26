module MatrixMetrics where
import qualified Numeric.LinearAlgebra.HMatrix as H

{--
This is a module for computing matrix metrics 
such as the spectrum of a graph.
--}

type DMatrix = H.Matrix Double
type HComplex = H.Complex Double

tetraH :: DMatrix
tetraH = (H.><) 4 4 [0,1,1,1,1,0,1,1,1,1,0,1,1,1,1,0]

-- spectra from tetraH: [(-1,3),(3,1)]
spectra :: (Eq a, Integral a) => DMatrix -> [(a, Int)]
spectra matrix = collect.rounded_reals.eigenlist $ matrix
  where
    eigenlist = (H.toList) . (H.eigenvalues)
    rounded_reals = map (round.H.realPart)
    collect [] = []
    collect (a:[]) = [(a, 1)]
    collect (a:as) = (a, length.filter (== a) $ (a:as)) :
                      (collect $ filter (/= a) as)


