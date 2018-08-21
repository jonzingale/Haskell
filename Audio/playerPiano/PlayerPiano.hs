module PlayerPiano where
import qualified Data.Vector.Unboxed as U
import BlinkyLights
import Sequencer
import Mobius
import Wave

freqPerSample :: Double -> Double
freqPerSample freq = freq * 2 * pi / 44100

ff :: Board -> Double -> Double
ff board t =
  let harms = U.map (\x -> sin (t * x)) $ U.fromList [1..12] in
  let tots = U.zipWith (*) board harms in
  U.sum tots / 20 -- U.sum board

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
  let bs = take 60 $ iterate update randos in -- too slow
  let them = map (s1 27.5 2 (maxBound `div` 2)) bs in
  let cs = take 240 $ paddedMobius in
  let us = map (s1 220 0.5 (maxBound `div` 8)) cs in
  -- makeWavFile $ U.concat us
  makeWavFile $ U.zipWith (+) (U.concat them) (U.concat us)
