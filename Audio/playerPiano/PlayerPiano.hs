module PlayerPiano where
import qualified Data.Vector.Unboxed as U
import Sequencer
import Wave

sine :: Frequency -> SamplesPerSec -> DurationSecs -> Volume -> VectSamples
sine freq samples len volume =
  let duration = round $ len * (fromIntegral samples) in
  let setVol = map (round . (* fromIntegral volume)) in
  let wSine = map sin [0.0, (freq * 2 * pi / (fromIntegral samples))..] in
  U.fromList $ take duration $ setVol $ wSine
   

main = makeWavFile $ sine 600 44100 3 (maxBound `div` 2)

