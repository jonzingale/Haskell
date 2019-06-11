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

-- Mixes three trajectories into a stereo file.
main = makeStereoWavFile left right
  where
    cs = (1,1,1)
    left = U.fromList $ map (round.lf) $ runLorenz 120 cs
    right = U.fromList $ map (round.rf) $ runLorenz 120 cs
    lf (x,y,z) = x * maxVal * 0.04 + 0.4 * y * maxVal * 0.03
    rf (x,y,z) = z * maxVal * 0.02 + 0.4 * y * maxVal * 0.03
