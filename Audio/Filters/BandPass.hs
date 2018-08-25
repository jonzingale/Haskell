{--
http://www.analog.com/media/en/technical-documentation/dsp-book/dsp_book_Ch16.pdf
--}
module Filters.BandPass where
import qualified Data.Vector.Unboxed as U
import Data.Int (Int32)
import System.Random
import Data.WAVE
import Wave

type SamplesR = U.Vector Double
type VectSamples = U.Vector Int32
type CutOffFreq = Double
type Q = Double

(mm, mm') = (100::Int, 100::Double)

blackman v j m = (* v) $ 0.42 - 0.5*cos(2*pi*j/m) + 0.08*cos(4*pi*j/m)
hamming v j m  = (* v) $ 0.54 - 0.46*cos(2*pi*j/m)

hh :: CutOffFreq -> SamplesR -- kernel
hh fc = normalize $ U.generate (mm+1) (g.fromIntegral)
  where
    normalize h = U.map (/ (U.sum h)) h

    g j | j == mm'/ 2 = blackman (2*pi*fc/44100) j mm'
        | otherwise =
          let val = sin(2*pi*(fc/44100) * (j-mm'/2)) / (j-mm'/2) in
          blackman val j mm'
          -- hamming val j mm'

specInv :: SamplesR -> SamplesR
specInv ss = U.map negate ss

mixBands :: SamplesR -> SamplesR -> SamplesR
mixBands ss tt = U.zipWith (+) ss tt -- may need normalized

bandPass :: Q -> CutOffFreq -> VectSamples -> VectSamples
bandPass q freq samples =
  let (low, hi) = (freq - q/2, freq + q/2) in
  let xx = (U.map fromIntegral samples)::SamplesR in
  let padx = (U.++) (U.replicate mm (0::Double)) xx in
  let lp = U.generate (U.length xx) (f padx (hh low)) in
  let hp = specInv $ U.generate (U.length xx) (f padx (hh hi)) in
  U.map floor $ U.drop mm $ specInv.mixBands lp $ hp
  where
    f x h j = sum [(U.!) x (j+mm-i) * (U.!) h i | i<-[0..mm]]

-- Helpers
test =
  let qs = [200, 400, 600, 1000, 2000, 5000, 10000] in
  let them = [bandPass 100] <*> qs <*> [randos] in
  makeWavFile $ U.concat them

testBandPass = do
  w <- getWAVEFile "blow.wav"
  let qs = [400, 600, 700, 800, 1000]
  let them = [bandPass 10] <*> qs <*> [unpack w]
  makeWavFile $ U.map (* 4) $ U.concat them

randos :: VectSamples
randos = (U.fromList).(take 22050) $ rs -- 1/2 second white noise
  where rs = randomRs (minBound, maxBound::Int32) $ mkStdGen 23

