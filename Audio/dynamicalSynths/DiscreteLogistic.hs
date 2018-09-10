module DiscreteLogistic where
import qualified Data.Vector.Unboxed as U
import Data.Int (Int32)
import System.Random
import Data.WAVE
import Wave

type Trajectory = [Population]
type Population = Double -- 0 < x < 1
type Rate = Double -- 0 < r < 4

{--
x(n+1) = r * xn * (1-xn)
--}

main r = -- main 3.77
  let xs = take 500 $ map (* 300) $ runLogistic r in -- scale populations
  let samples = map (s1 0.15 (maxBound `div` 2)) xs in
  makeWavFile $ U.concat samples

runLogistic :: Rate -> Trajectory
runLogistic r = iterate (logistic r) (1 / exp 1)

logistic :: Rate -> Population -> Population
logistic r xn = r * xn * (1-xn)

freqPerSample :: Double -> Double
freqPerSample freq = freq * 2 * pi / 44100

s1 :: DurationSecs -> Volume -> Frequency -> VectSamples
s1 len volume freq =
  let setVol = U.map (round . (* fromIntegral volume)) in
  let sine = map (\x -> sin x) [0.0, freqPerSample freq..] in
  let duration = take.round $ len * 44100 in
  setVol $ U.fromList $ duration sine

dispLogistic r = putStr.unlines.map show $ runLogistic r
