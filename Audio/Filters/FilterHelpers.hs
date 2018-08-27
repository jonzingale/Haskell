module Filters.FilterHelpers where
import qualified Data.Vector.Unboxed as U
import Filters.ConvolutionFilters (bandPass, lowPass, highPass)
import Filters.FFTFilters (fftLowPass, fftHighPass, fftBandPass)
import Filters.ComonadicFilters (lowPassW)
import Filters.HiPass (hiPass)
import Data.Int (Int32)
import System.Random
import Filters.Wave
import Data.WAVE

testHigh = testFilter highPass
testBand = testFilter (bandPass 100)
testLow  = testFilter lowPass

 -- 20 secs for 6 secs audio
testFFTHigh = makeWavFile $ fftHighPass 440 randTwos
testFFTBand = makeWavFile $ fftBandPass 50 440 randTwos
testFFTLow  = makeWavFile $ fftLowPass 440 randTwos

testComonadic = -- 2 seconds for 6 seconds of 440 filtered noise
  let rs = take (44100*6) $ randomRs dynRange $ mkStdGen 23 in
  makeWavFile $ U.fromList $ lowPassW 440 rs

testFilter filter =
  let qs = [200, 400, 600, 4000, 6000, 8000] in
  let them = [filter] <*> (qs ++ reverse qs) <*> [randos] in
  makeWavFile $ U.concat them

dynRange :: (Int32, Int32)
dynRange = (minBound, maxBound)

randos :: VectSamples
randos = (U.fromList).(take (2^14)) $ rs -- power of 2 under 22050 white noise
  where rs = randomRs dynRange $ mkStdGen 23

randTwos :: VectSamples -- power of 2 necessary for FFT
randTwos = (U.fromList).(take (2^18)) $ rs -- ~6 seconds white noise
  where rs = randomRs dynRange $ mkStdGen 23
