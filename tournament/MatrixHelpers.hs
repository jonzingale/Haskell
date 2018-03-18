module MatrixHelpers where
import qualified Numeric.LinearAlgebra.HMatrix as H
import MatrixMetrics
import Text.Printf

-- Alec's Lights
lights :: [[Double]]
lights =[[1,1,0,0,0,0,0,0],
         [1,1,1,0,0,0,0,0],
         [0,1,1,1,0,0,0,0],
         [0,0,1,1,1,0,0,0],
         [0,0,0,1,1,1,0,0],
         [0,0,0,0,1,1,1,0],
         [0,0,0,0,0,1,1,1],
         [0,0,0,0,0,0,1,1]]

lightsToMatrix :: String
lightsToMatrix = -- tests rank on AlecLights
  let flatLights = foldr (++) [] lights in
  let graph = (H.><) 8 8 flatLights in
  printf format (H.rank graph) (show.spectra $ graph)
    where format = "rank: %i, spec: %s"

 -- Given k rounds, return the spectra for n players.
roundSpectra k = [toSpectra.nKs (n+1) $ k | n<-[2..]]
  where
    nKs n = (take n).repeat.fromIntegral
    toSpectra = spectra.symmetric
