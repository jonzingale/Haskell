module Filters.FilterHelpers where
import qualified Data.Vector.Unboxed as U
import Filters.ConvolutionFilters (bandPass, lowPass, highPass)
import Filters.FFTFilters (fftLowPass, fftHighPass, fftBandPass)
import Filters.HiPass (hiPass)
import Data.Int (Int32)
import System.Random
import Filters.Wave
import Data.WAVE

testHigh = testFilter highPass
testBand = testFilter (bandPass 50)
testLow  = testFilter lowPass

 -- 20 secs for 6 secs audio
testFFTHigh = makeWavFile $ fftHighPass 440 randTwos
testFFTBand = makeWavFile $ fftBandPass 50 440 randTwos
testFFTLow  = makeWavFile $ fftLowPass 440 randTwos

testFilter filter =
  let qs = [200, 400, 600, 4000, 6000, 8000] in
  let them = [filter] <*> (qs ++ reverse qs) <*> [randos] in
  makeWavFile $ U.concat them

randos :: VectSamples
randos = (U.fromList).(take 22050) $ rs -- 1/2 second white noise
  where rs = randomRs (minBound, maxBound::Int32) $ mkStdGen 23

randTwos :: VectSamples -- power of 2 necessary for FFT
randTwos = (U.fromList).(take (2^18)) $ rs -- ~6 seconds white noise
  where rs = randomRs (minBound, maxBound::Int32) $ mkStdGen 23