module Pandemic where
import qualified Data.Vector.Unboxed as U
import Wave (VectSamples)
import Data.Int (Int32)
import Peptide
import Event
-- import Main

{--
Here is where the artistic choices for composition live.

TODO:
- Stringed Instruments: Noise
- long vs. short tones
- frequency ranges
- Harmonics?? maybe in Event?
- Mixer
- parallelized components

Peptide lengths
[4406,2596,1274,420,276,223,122,122,62,25,21,19,17,14,12,11,10,4,2]

Possible Peptide partition
4406,2596,1274, -- fast blips, dense short tones
420,276,223, -- melodies, short tones
122,122,62, -- strings, long tones
25,21,19,17,14,12,11,10,4,2 -- sparse short tones
--}

type Scalar = Double

-- peptide length used as origin
center :: Double
center = 420.0

scale :: Peptide -> Scalar
scale p = (fromIntegral.length.peptideToSound $ p) / center

longTones :: Sound -> Scalar -> VectSamples
longTones (freq, epoch, dur) k =
  let vol = maxBound `div` 1 :: Int32 in
  let setVol = U.map (round . (* fromIntegral vol)) in
  let (eSec, dSec) = durations epoch dur in
  let sine = map sin [0.0, freqPerSample freq..] in
  let noteTime = take.round $ eSec * 44100 * k in -- convolve with inv exp
  let restTime = take (round $ dSec * 44100 * k) (repeat 0) in
  setVol $ U.fromList $ noteTime sine ++ restTime

-- toSound :: Sound -> VectSamples
-- toSound (freq, epoch, dur) =
--   let vol = maxBound `div` 1 :: Int32 in
--   let setVol = U.map (round . (* fromIntegral vol)) in
--   let (eSec, dSec) = durations epoch dur in
--   let sine = map sin [0.0, freqPerSample freq..] in
--   let noteTime = take.round $ eSec * 44100 in -- convolve with inv exp
--   let restTime = take (round $ dSec * 44100) (repeat 0) in
--   setVol $ U.fromList $ noteTime sine ++ restTime
