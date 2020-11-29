module PlayerPiano where
import qualified Data.Vector.Unboxed as U
import BlinkyLights
-- import Sequencer -- not used!
import Mobius
import Wave

freqPerSample :: Double -> Double
freqPerSample freq = freq * 2 * pi / 44100

-- zip binaryList with list of harmonics and returns
-- the value at point on a complex waveform.
ff :: Board -> Double -> Double
ff board t =
  let harms = U.map (\x -> sin (t * x)) $ U.fromList [1..12] in
  let tots = U.zipWith (*) board harms in
  U.sum tots / 20 -- U.sum board

gg :: Board -> Double -> Double
gg board t =
  -- harmonics summed and then normalized by (/ n)
  (/ 5) $ sin (t/4) + sin (t/2) + sin (t) + sin (t*2) + sin (t/3)

s1 :: Frequency -> DurationSecs -> Volume -> Board -> VectSamples
s1 freq len volume bb =
  let setVol = U.map (round . (* fromIntegral volume)) in
  let sine = map (ff bb) [0.0, freqPerSample freq..] in
  let duration = take.round $ len * 44100 in
  setVol $ U.fromList $ duration sine

main =
  let bs = take 120 $ iterate update randos in -- too slow
  let them = map (s1 27.5 2 (maxBound `div` 2)) bs in
  let cs = take 960 $ paddedMobius in
  let us = map (s1 220 0.25 (maxBound `div` 5)) cs in
  makeStereoWavFile (U.concat them) (U.concat us) -- stereo
  -- makeWavFile $ U.zipWith (+) (U.concat them) (U.concat us) -- mono
