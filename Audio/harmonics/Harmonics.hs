module Harmonics where
import qualified Data.Vector.Unboxed as U
import Wave (Frequency, stereopack)
import Data.Int (Int32)
import System.Random
import Data.WAVE
import Types

freqPerSample :: Double -> Double
freqPerSample freq = freq * 2 * pi / 44100

toNoiseTimbreEven :: Frequency -> [Double]
toNoiseTimbreEven freq =
  let vol x = (* (exp (-0.8*x))) in
  let nn x = map (vol x) (randomRs (0, 1::Double) $ mkStdGen 32) in
  let ss x = map ((vol x).sin) [0.0, freqPerSample (2*x*freq)..] in
  let sound = foldr (zipWith (+)) (repeat 0) [ss t | t <- [1..20]] in
  zipWith (+) sound (nn 2)

toNoiseTimbreOdd :: Frequency -> [Double]
toNoiseTimbreOdd freq =
  let vol x = (* (exp (-0.8*x))) in
  let nn x = map (vol x) (randomRs (0, 1::Double) $ mkStdGen 32) in
  let ss x = map ((vol x).sin) [0.0, freqPerSample ((2*x+1)*freq)..] in
  let sound = foldr (zipWith (+)) (repeat 0) [ss t | t <- [1..20]] in
  zipWith (+) sound (nn 2)

toEvenTimbre :: Frequency -> [Double]
toEvenTimbre freq =
  let vol x = (* (exp (-0.8*x))) in
  let ss x = map ((vol x).sin) [0.0, freqPerSample (2*x*freq)..] in
  foldr (zipWith (+)) (repeat 0) [ss t | t <- [1..20]] -- Note: must be finite

toOddTimbre :: Frequency -> [Double]
toOddTimbre freq =
  let vol x = (* (exp (-0.7*x))) in
  let ss x = map ((vol x).sin) [0.0, freqPerSample ((2*x+1)*freq)..] in
  foldr (zipWith (+)) (repeat 0) [ss t | t <- [1..20]] -- Note: must be finite
