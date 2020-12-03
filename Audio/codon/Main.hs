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

toSound :: Sound -> VectSamples
toSound (freq, epoch, dur) =
  let vol = maxBound `div` 4 :: Int32 in
  let setVol = U.map (round . (* fromIntegral vol)) in
  let (eSec, dSec) = durations epoch dur in
  let sine = map sin [0.0, freqPerSample freq..] in
  let noteTime = take.round $ eSec * 44100 in -- convolve with inv exp
  let restTime = take (round $ dSec * 44100) (repeat 0) in
  setVol $ U.fromList $ noteTime sine ++ restTime
  where
    durations e d
      | e < d = (toTime e, toTime e)
      | otherwise = (toTime d, toTime e - toTime d)

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
scale p = center / (fromIntegral.length.peptideToSound $ p)

-- Volume is counterintuitive, Int is a divisor.
longTones :: Scalar -> Volume -> Octave -> Sound -> VectSamples
longTones k vol oct (freq, epoch, dur) =
  let octave = 2**oct in
  let volume = maxBound `div` vol :: Int32 in
  let setVol = U.map (round . (* fromIntegral volume)) in

  let (eSec, dSec) = durations epoch dur in
  let noteTime = take.round $ eSec * 44100 * k in -- convolve with inv exp
  let restTime = take (round $ dSec * 44100 * k) (repeat 0) in

  let sine = map sin [0.0, freqPerSample (freq * octave)..] in
  setVol $ U.fromList $ noteTime sine ++ restTime
  where
    durations e d
      | e < d = (toTime e, toTime e)
      | otherwise = (toTime d, toTime e - toTime d)

shortTones :: Scalar -> Volume -> Octave -> Sound -> VectSamples
shortTones k vol oct (freq, epoch, dur) =
  let octave = 2**oct in
  let volume = maxBound `div` vol :: Int32 in -- 2147483647
  let setVol = U.map (round . (* fromIntegral volume)) in

  let (eSec, dSec) = durations epoch dur k in -- simplify
  let noteTime = take.round $ eSec * 44100 in -- convolve with inv exp
  let restTime = take (round $ dSec * 44100) (repeat 0) in

  let sine = map sin [0.0, freqPerSample (freq * octave)..] in
  setVol $ U.fromList $ noteTime sine ++ restTime
  where
    durations e d k
      | e < d = (toTime e, toTime e)
      | otherwise = (toTime d, k * toTime e - toTime d)

-- Todo: Write a reasonable mixer
main = do
  datum <- readFile "./covid_cdna.txt"
  let dna = concat.words $ datum

  -- fast sound : 4406
  let peptide = (!! 1) $ extractPeptides dna
  let k = scale peptide
  -- :: Scalar -> Volume -> Octave -> Sound -> VectSamples
  let s1 =  U.concat $ map (shortTones k 13 5) $ peptideToSound peptide
  let peptide = (!! 5) $ extractPeptides dna
  let k = scale peptide
  let s6 =  U.concat $ map (shortTones k 13 5) $ peptideToSound peptide
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
  let peptide = (!! 0) $ extractPeptides dna
  let k = scale peptide
  let s10 = U.concat $ map (shortTones k 13 2) $ peptideToSound peptide

  let peptide = (!! 2) $ extractPeptides dna
  let k = scale peptide
  let s11 = U.concat $ map (shortTones k 13 3) $ peptideToSound peptide

  let peptide = (!! 3) $ extractPeptides dna
  let k = scale peptide
  let s12 = U.concat $ map (shortTones k 13 3) $ peptideToSound peptide

  let peptide = (!! 15) $ extractPeptides dna
  let k = scale peptide
  let s13 = U.concat $ map (shortTones k 13 3) $ peptideToSound peptide

  let peptide = (!! 18) $ extractPeptides dna
  let k = scale peptide
  let s14 = U.concat $ map (shortTones k 13 4) $ peptideToSound peptide

  let peptide = (!! 10) $ extractPeptides dna
  let k = scale peptide
  let s15 = U.concat $ map (shortTones k 13 3) $ peptideToSound peptide

  -- 11, 12, 13, 17 remaining
  let shorts = U.zipWith (+) s15 $ U.zipWith (+) s14 $ U.zipWith (+) s13 $ U.zipWith (+) s12 $ U.zipWith (+) s10 s11

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
  let s5 =  U.concat $ map (longTones k 12 2) $ peptideToSound peptide
  let s165 = U.zipWith (+) s16 s5

  let sl = U.zipWith (+) shorts $ U.zipWith (+) s9 $ U.zipWith (+) s8 $ U.zipWith (+) s165 s2
  let sr = U.zipWith (+) shorts $ U.zipWith (+) s9 $ U.zipWith (+) s8 $ U.zipWith (+) (U.zipWith (+) s34 s2) s7

  -- mix
  makeStereoWavFile (lowPass 3000 sl) (lowPass 3000 sr)
