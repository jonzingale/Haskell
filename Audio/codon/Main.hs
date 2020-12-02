module Main where
import qualified Data.Vector.Unboxed as U
import AminoAcidToPitch (frequency)
import AminoAcidToPitch (Freq)
import Data.Int (Int32)
import Peptide
import Event
import Wave

import Filters (highPass, lowPass)

type Sound = (Freq, Epoch, Duration)

-- 
freqPerSample :: Double -> Double
freqPerSample freq = freq * 2 * pi / 44100

toTime :: Duration -> DurationSecs
toTime Eighth = 0.125
toTime Quarter = 0.25
toTime Half = 0.5
toTime Whole = 1

durations :: Epoch -> Duration -> (DurationSecs, DurationSecs)
durations e d
  | e < d = (toTime e, toTime e)
  | otherwise = (toTime d, toTime e - toTime d)

toSound :: Sound -> VectSamples
toSound (freq, epoch, dur) =
  let vol = maxBound `div` 5 :: Int32 in
  let setVol = U.map (round . (* fromIntegral vol)) in
  let (eSec, dSec) = durations epoch dur in
  let sine = map sin [0.0, freqPerSample freq..] in
  let noteTime = take.round $ eSec * 44100 in -- convolve with inv exp
  let restTime = take (round $ dSec * 44100) (repeat 0) in
  setVol $ U.fromList $ noteTime sine ++ restTime

peptideToSound :: Peptide -> [Sound]
peptideToSound peptide =
  [(frequency.pitch $ c, epoch c, duration c) | c <- peptideToEvents peptide]

-- main = do
--   datum <- readFile "./covid_cdna.txt"
--   let dna = concat.words $ datum
--   let peptide = (!! 12) $ extractPeptides dna
--   let sound =  map toSound $ peptideToSound peptide
--   makeWavFile $ lowPass 880 $ U.concat sound

--------
type Scalar = Double
type Octave = Double

-- peptide length used as origin
center :: Double
center = 420.0

scale :: Peptide -> Scalar
scale p = (fromIntegral.length.peptideToSound $ p) / center

-- Volume is counterintuitive, Int is a divisor.
longTones :: Scalar -> Volume -> Octave -> Sound -> VectSamples
longTones k vol oct (freq, epoch, dur) =
  let octave = 2**oct in
  let volume = maxBound `div` vol :: Int32 in
  let setVol = U.map (round . (* fromIntegral volume)) in
  let sine = map sin [0.0, freqPerSample (freq * octave)..] in
  let (eSec, dSec) = durations epoch dur in
  let noteTime = take.round $ eSec * 44100 / k in -- convolve with inv exp
  let restTime = take (round $ dSec * 44100 / k) (repeat 0) in
  setVol $ U.fromList $ noteTime sine ++ restTime

shortTones :: Scalar -> Volume -> Octave -> Sound -> VectSamples
shortTones k vol oct (freq, epoch, dur) =
  let octave = 2**oct in
  let volume = maxBound `div` vol :: Int32 in
  let setVol = U.map (round . (* fromIntegral volume)) in
  let sine = map sin [0.0, freqPerSample (freq * octave)..] in
  let (eSec, dSec) = durations epoch dur in
  let noteTime = take.round $ eSec * 44100 / k in -- convolve with inv exp
  let restTime = take (round $ dSec * 44100 / k) (repeat 0) in
  setVol $ U.fromList $ noteTime sine ++ restTime

-- Todo: Write a reasonable mixer
main = do
  datum <- readFile "./covid_cdna.txt"
  let dna = concat.words $ datum

  -- fast sound : 4406
  let peptide = (!! 1) $ extractPeptides dna
  let k = scale peptide
  let s1 =  U.concat $ map (longTones k 8 3) $ peptideToSound peptide
  let peptide = (!! 5) $ extractPeptides dna
  let k = scale peptide
  let s6 =  U.concat $ map (longTones k 9 3) $ peptideToSound peptide
  let s16 = (U.zipWith (+) s1 s6)

  let peptide = (!! 6) $ extractPeptides dna
  let k = scale peptide
  let s7 =  U.concat $ map (longTones k 7 3) $ peptideToSound peptide

  -- avg sound : 420
  let peptide = (!! 14) $ extractPeptides dna
  let s2 =  U.concat $ map toSound $ peptideToSound peptide

  let peptide = (!! 7) $ extractPeptides dna
  let s8 =  U.concat $ map toSound $ peptideToSound peptide

  let peptide = (!! 9) $ extractPeptides dna
  let s9 =  U.concat $ map toSound $ peptideToSound peptide

  -- sparse sounds, 2nd octave? dsec, esec


  -- slow sounds : 25, 17
  let peptide = (!! 4) $ extractPeptides dna
  let k = scale peptide
  let s3 =  U.concat $ map (longTones k 9 1) $ peptideToSound peptide

  let peptide = (!! 8) $ extractPeptides dna
  let k = scale peptide
  let s4 =  U.concat $ map (longTones k 7 1) $ peptideToSound peptide
  let s34 = (U.zipWith (+) s3 s4)

  let peptide = (!! 16) $ extractPeptides dna
  let k = scale peptide
  let s5 =  U.concat $ map (longTones k 10 2) $ peptideToSound peptide
  let s165 = U.zipWith (+) s16 s5

  let sl = U.zipWith (+) s9 $ U.zipWith (+) s8 $ U.zipWith (+) s165 s2
  let sr = U.zipWith (+) s9 $ U.zipWith (+) s8 $ U.zipWith (+) (U.zipWith (+) s34 s2) s7

  -- mix
  makeStereoWavFile (lowPass 880 sl) (lowPass 880 sr)
