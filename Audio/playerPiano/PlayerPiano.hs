module PlayerPiano where
import qualified Data.Vector.Unboxed as U
import BlinkyLights
import Sequencer
import Wave

freqPerSample :: Double -> Double
freqPerSample freq = freq * 2 * pi / 44100

ff :: Board -> Double -> Double
ff board t =
  let harms = U.map (\x -> sin (t * x)) $ U.fromList [1..10] in
  let tots = U.zipWith (*) board harms in
  U.sum tots / U.sum board

gg :: Board -> Double -> Double
gg board t =
  (/ 5) $ sin (t/4) + sin (t/2) + sin (t) + sin (t*2) + sin (t/3)

s1 :: Frequency -> DurationSecs -> Volume -> Board -> VectSamples
s1 freq len volume bb =
  let setVol = map (round . (* fromIntegral volume)) in
  let sine = map (ff bb) [0.0, freqPerSample freq..] in
  let duration = take.round $ len * 44100 in
  U.fromList.duration.setVol $ sine

main =
  let bs = take 30 $ iterate update initState in -- too slow
  let them = map (s1 50 1 (maxBound `div` 2)) bs in
  makeWavFile $ U.concat them
