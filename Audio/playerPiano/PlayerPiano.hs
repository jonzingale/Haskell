module PlayerPiano where
import qualified Data.Vector.Unboxed as U
import BlinkyLights
import Sequencer
import Wave

freqPerSample :: Double -> Double
freqPerSample freq = freq * 2 * pi / 44100

ff :: Board -> Double -> Double
ff board t =
  let harms = U.map (\x -> sin t / x) $ U.fromList [1..100] in
  let tots = U.zipWith (*) board harms in
  U.sum tots / U.sum board

s1 :: Frequency -> DurationSecs -> Volume -> Board -> VectSamples
s1 freq len volume bb =
  let setVol = map (round . (* fromIntegral volume)) in
  let sine = map (ff bb) [0.0, freqPerSample freq..] in
  let duration = take.round $ len * 44100 in
  U.fromList.duration.setVol $ sine

main =
  let bs = take 20 $ iterate update initState in -- too slow
  let them = map (s1 600 0.2 (maxBound `div` 2)) bs in

  makeWavFile $ U.concat them

-- mix :: VectSamples -> VectSamples -> VectSamples
-- mix a b = U.zipWith (+) (half a) (half b)
--   where half = U.map $ flip div 2

