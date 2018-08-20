module PlayerPiano where
import qualified Data.Vector.Unboxed as U
import Data.Int (Int32)
import Sequencer
import Math.FFT
import Wave

import Data.Complex
import Data.Array.CArray
import qualified Data.Array.IArray as Arr

-- towards fft

rowByRow :: [Complex Int32]
rowByRow = map (:+ 0) $ [440]

carray :: [Complex Double] -> CArray Int (Complex Double)
carray v = Arr.listArray (0, 44100) v

test =
  let w1 = s1 50 3 (maxBound `div` 2)  in
  let w = (map (:+ 0) $ U.toList $ U.map fromIntegral w1)::[Complex Double] in
  dft.carray $ w

test2 =
  let w1 = s1 50 3 (maxBound `div` 2)  in
  let w = (map (:+ 0) $ U.toList $ U.map fromIntegral w1)::[Complex Double] in
  idft.carray $ w
--

freqPerSample :: Double -> Double
freqPerSample freq = freq * 2 * pi / 44100

ff t = (/ 3) $ sin(t) + sin(t/3) + sin(t/4)

s1 :: Frequency -> DurationSecs -> Volume -> VectSamples
s1 freq len volume =
  let setVol = map (round . (* fromIntegral volume)) in
  let sine = map sin [0.0, freqPerSample freq..] in
  let duration = take.round $ len * 44100 in
  U.fromList.duration.setVol $ sine

s2 :: Frequency -> DurationSecs -> Volume -> VectSamples
s2 freq len volume =
  let setVol = map (round . (* fromIntegral volume)) in
  let sine = map ff [0.0, freqPerSample freq..] in
  let duration = take.round $ len * 44100 in
  U.fromList.duration.setVol $ sine

mix :: VectSamples -> VectSamples -> VectSamples
mix a b = U.zipWith (+) (half a) (half b)
  where half = U.map $ flip div 2

main =
  let w1 = s1 300 3 (maxBound `div` 8) in
  let w2 = s2 600 3 (maxBound `div` 2) in
  makeWavFile $ mix w1 w2

