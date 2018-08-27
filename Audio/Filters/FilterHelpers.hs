module Filters.FilterHelpers where
import qualified Data.Vector.Unboxed as U
import Filters.ConvolutionFilters (bandPass, lowPass, highPass)
import Filters.HiPass (hiPass)
import Filters.FFTFilters (fftFilter)
import Data.Int (Int32)
import System.Random
import Filters.Wave
import Data.WAVE

testBand =
  let qs = [200, 400, 600, 4000, 6000, 8000] in
  let them = [bandPass 100] <*> (qs ++ reverse qs) <*> [randos] in
  makeWavFile $ U.concat them

testLow =
  let qs = [200, 400, 600, 4000, 6000, 8000] in
  let them = [lowPass] <*> (qs ++ reverse qs) <*> [randos] in
  makeWavFile $ U.concat them

randos :: VectSamples
randos = (U.fromList).(take 22050) $ rs -- 1/2 second white noise
  where rs = randomRs (minBound, maxBound::Int32) $ mkStdGen 23

testFFT =
  let rs = U.take (2^14) $ randos in
  makeWavFile $ fftFilter rs