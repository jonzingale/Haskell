module Thrush where
import qualified Data.Vector.Unboxed as U
import Data.Int (Int32)
import Data.WAVE
import Lorenz
import Wave

{--
This code is designed to construct manifolds
from bird songs via Taken's method.
--}

thrush = "HermitThrush.wav"

viewThrush = viewSamples thrush

maxVal = (2^31-1)/3::Double -- averages 3 signals

-- Mixes three trajectories into a stereo file.
main = makeStereoWavFile left right
  where
    cs = (1,1,1) -- arbitrary initial coordinates.
    left  = U.fromList $ map (round.lf) $ runLorenz 120 cs
    right = U.fromList $ map (round.rf) $ runLorenz 120 cs
    lf (x,y,z) = x * maxVal * 0.04 + 0.4 * y * maxVal * 0.03
    rf (x,y,z) = z * maxVal * 0.02 + 0.4 * y * maxVal * 0.03

-- reconstruction X,Y pairs
seriesPair :: [(Double, Double)]
seriesPair =
  let delay = take 100 $ repeat 0 in
  let xs = map x_val $ runLorenz 1 (1,1,1) in
  zip xs (delay ++ xs)

