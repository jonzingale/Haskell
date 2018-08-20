module PlayerPiano where
import qualified Data.Vector.Unboxed as U
import Data.Int (Int32)
import Sequencer
import Samples
import Wave

type Frequency = Double
type SamplesPerSec = Int
type DurationSecs = Double
type Sample = [Int32]
type Volume = Int32

sine :: Frequency -> SamplesPerSec -> DurationSecs -> Volume -> VectSamples
sine freq samples len volume =
  let duration = round $ len * (fromIntegral samples) in
  let setVol = map (round . (* fromIntegral volume)) in
  let wSine = map sin [0.0, (freq * 2 * pi / (fromIntegral samples))..] in
  U.fromList $ take duration $ setVol $ wSine
   

main = makeWavFile $ sine 600 44100 3 (maxBound `div` 2)

