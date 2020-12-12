module Harmonics where
import System.Random
import Types

freqPerSample :: Double -> Double
freqPerSample freq = freq * 2 * pi / 44100

noiseTimbreEven :: Timbre
noiseTimbreEven freq =
  let vol x = (* (exp (-0.8*x))) in
  let nn x = map (vol x) (randomRs (0, 1::Double) $ mkStdGen 32) in
  let ss x = map ((vol x).sin) [0.0, freqPerSample (2*x*freq)..] in
  let sound = foldr (zipWith (+)) (repeat 0) [ss t | t <- [1..20]] in
  zipWith (+) sound (nn 2)

noiseTimbreOdd :: Timbre
noiseTimbreOdd freq =
  let vol x = (* (exp (-0.8*x))) in
  let nn x = map (vol x) (randomRs (0, 1::Double) $ mkStdGen 32) in
  let ss x = map ((vol x).sin) [0.0, freqPerSample ((2*x+1)*freq)..] in
  let sound = foldr (zipWith (+)) (repeat 0) [ss t | t <- [1..20]] in
  zipWith (+) sound (nn 2)

evenTimbre :: Timbre
evenTimbre freq =
  let vol x = (* (exp (-0.8*x))) in
  let ss x = map ((vol x).sin) [0.0, freqPerSample (2*x*freq)..] in
  foldr (zipWith (+)) (repeat 0) [ss t | t <- [1..20]] -- Note: must be finite

oddTimbre :: Timbre
oddTimbre freq =
  let vol x = (* (exp (-0.7*x))) in
  let ss x = map ((vol x).sin) [0.0, freqPerSample ((2*x+1)*freq)..] in
  foldr (zipWith (+)) (repeat 0) [ss t | t <- [1..20]] -- Note: must be finite

squareTimbre :: Timbre
squareTimbre freq = -- NOTE: const 6 is for overall attenuation
  let ss x = map ((/ (6*x)).sin) [0.0, freqPerSample ((2*x+1)*freq)..] in
  foldr (zipWith (+)) (repeat 0) [ss t | t <- [1..20]] -- Note: must be finite

nonSquareTimbre :: Timbre
nonSquareTimbre freq =
  let ss x = map ((/ (6*x)).sin) [0.0, freqPerSample (2*x*freq)..] in
  foldr (zipWith (+)) (repeat 0) [ss t | t <- [1..20]] -- Note: must be finite

-- not yet working, probably negative values
sawTimbre :: Timbre
sawTimbre freq =
  let ss x = map sin [0.0, freqPerSample (x*freq)..] in
  let tt = map (\x -> ((-1)**x) / x) in
  let rr = map (\x -> 0.5 - x) in
  foldr (zipWith (+)) (repeat 0) [ rr.tt.ss $ t | t <- [1..20]]
