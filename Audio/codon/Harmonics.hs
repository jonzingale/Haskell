module Harmonics where
import qualified Data.Vector.Unboxed as U
import CompositionHelpers hiding (toSound)
import Wave (Frequency, stereopack)
import Peptide (extractPeptides)
import Data.Int (Int32)
import System.Random
import Data.WAVE

type VectSamples = U.Vector Int32
type Timbre = Frequency -> [Double]

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

toSound :: Timbre -> Sound -> VectSamples
toSound timbre (freq, epoch, dur) =
  let vol = maxBound `div` 4 :: Int32 in
  let setVol = U.map (round . (* fromIntegral vol)) in
  let (eSec, dSec) = durations epoch dur in
  let noteTime = take.round $ eSec * 44100 in
  let restTime = take (round $ dSec * 44100) $ repeat 0 in
  let harmonicSine = timbre freq :: [Double] in
  setVol $ U.fromList $ noteTime harmonicSine ++ restTime
  where
    durations e d
      | e < d = (toTime e, toTime e)
      | otherwise = (toTime d, toTime e - toTime d)

example = do -- creates melody with harmonic content
  datum <- readFile "./covid_cdna.txt"
  let dna = concat.words $ datum
  let peptide = (!! 14) $ extractPeptides dna
  let s1 =  U.concat $ map (toSound toNoiseTimbreEven) $ peptideToSound peptide
  let s2 =  U.concat $ map (toSound toNoiseTimbreOdd)  $ peptideToSound peptide
  putWAVEFile "harmonics.wav" $ stereopack s1 s2
