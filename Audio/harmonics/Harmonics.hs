module Harmonics where
import System.Random
import Types

-- TODO:
-- 1. end sample on 0.0
-- 2. sum fidelity while under nyquist limit
-- 3. modulate from timbre1 to timbre2 over melody
-- 4. fully unbox Timbres
-- 5. normalize timbres

fidelity = 20

freqPerSample :: Double -> Double
freqPerSample freq = freq * 2 * pi / 44100

noiseTimbreEven :: Timbre
noiseTimbreEven freq =
  let vol x = (* (exp (-0.8*x))) in
  let nn x = map (vol x) (randomRs (0, 1::Double) $ mkStdGen 32) in
  let ss x = map ((vol x).sin) [0.0, freqPerSample (2*x*freq)..] in
  let sound = foldr (zipWith (+)) (repeat 0) [ss t | t <- [1..fidelity]] in
  zipWith (+) sound (nn 2)

noiseTimbreOdd :: Timbre
noiseTimbreOdd freq =
  let vol x = (* (exp (-0.8*x))) in
  let nn x = map (vol x) (randomRs (0, 1::Double) $ mkStdGen 32) in
  let ss x = map ((vol x).sin) [0.0, freqPerSample ((2*x+1)*freq)..] in
  let sound = foldr (zipWith (+)) (repeat 0) [ss t | t <- [1..fidelity]] in
  zipWith (+) sound (nn 2)

evenTimbre :: Timbre
evenTimbre freq =
  let vol x = (* (exp (-0.8*x))) in
  let ss x = map ((vol x).sin) [0.0, freqPerSample (2*x*freq)..] in
  foldr (zipWith (+)) (repeat 0) [ss t | t <- [1..fidelity]]

oddTimbre :: Timbre
oddTimbre freq =
  let vol x = (* (exp (-0.8*x))) in
  let ss x = map ((vol x).sin) [0.0, freqPerSample ((2*x+1)*freq)..] in
  foldr (zipWith (+)) (repeat 0) [ss t | t <- [1..fidelity]]

-- sin(x) + 1⁄3sin(3x) + 1⁄5sin(5x) + ...
squareTimbre :: Timbre
squareTimbre freq = -- NOTE: const 6 is for overall attenuation
  let ss x = map ((/ (6*x)).sin) [0.0, freqPerSample ((2*x+1)*freq)..] in
  foldr (zipWith (+)) (repeat 0) [ss t | t <- [1..fidelity]]

nonSquareTimbre :: Timbre
nonSquareTimbre freq =
  let ss x = map ((/ (6*x)).sin) [0.0, freqPerSample (2*x*freq)..] in
  foldr (zipWith (+)) (repeat 0) [ss t | t <- [1..fidelity]]

-- sin(x) - 1⁄2sin(2x) + 1⁄3sin(3x) - 1⁄4sin(4x) + 1⁄5sin(5x) - ...
sawTimbre :: Timbre
sawTimbre freq =
  let ss n = map sin [0.0, freqPerSample (n*freq)..] in -- sin(nx)
  let tt n = map (\t -> 0.5 - t * ((-1)**n) * (1/n)) in -- (+/-) 1/n
  let vv = map (/ 8) in -- volume
  foldr (zipWith (+)) (repeat 0) [ vv.tt n $ ss n | n <- [1..fidelity]]
