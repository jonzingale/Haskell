module Harmonics where
import Types

{--
Note: Sounds are expected to be normalized on the interval [-1, 1].
--}

-- TODO:
-- 1. end sample on 0.0
-- 2. sum fidelity while under nyquist limit
-- 3. modulate from timbre1 to timbre2 over melody
-- 4. fully unbox Timbres
-- 5. normalize timbres

fidelity :: Double
fidelity = 20

freqPerSample :: Double -> Double
freqPerSample freq = freq * 2 * pi / 44100

-- Timbres.

emptyTimbre :: Timbre
emptyTimbre freq = map sin [0.0, freqPerSample freq..]

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
  let tt n = map (\t -> t * ((-1)**n) * (1/n)) in -- (+/-) 1/n
  let vv = map (/ 8) in -- volume
  foldr (zipWith (+)) (repeat 0) [ vv.tt n $ ss n | n <- [1..fidelity]]
