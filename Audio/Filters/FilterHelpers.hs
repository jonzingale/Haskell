module Filters.FilterHelpers where
import qualified Data.Vector.Unboxed as U
import Filters.BandPass (bandPass)
import Filters.LowPass (lowPass)
import Filters.HiPass (hiPass)
import Data.Int (Int32)
import System.Random
import Filters.Wave
import Data.WAVE

test =
  let qs = [200, 400, 600, 4000, 6000, 8000] in
  let them = [bandPass 100] <*> (qs ++ reverse qs) <*> [randos] in
  makeWavFile $ U.concat them

testBandPass = do
  w <- getWAVEFile "blow.wav"
  let qs = [400, 600, 700, 800, 1000]
  let them = [bandPass 10] <*> qs <*> [unpack w]
  makeWavFile $ U.map (* 4) $ U.concat them

randos :: VectSamples
randos = (U.fromList).(take 22050) $ rs -- 1/2 second white noise
  where rs = randomRs (minBound, maxBound::Int32) $ mkStdGen 23