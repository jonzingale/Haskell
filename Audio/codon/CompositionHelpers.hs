module CompositionHelpers where
import qualified Data.Vector.Unboxed as U
import AminoAcidToPitch (frequency)
import Filters (highPass, lowPass)
import AminoAcidToPitch (Freq)
import Data.Int (Int32)
import Data.WAVE
import Peptide
import Event
import Wave

type Sound = (Freq, Epoch, Duration)
type Tone = (Scalar -> Volume -> Octave -> Sound -> VectSamples)
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

-- Note: high octaves strongly attenuated by lowPass above 1500
simpleShortFile :: Int -> Tone -> Double -> IO ()
simpleShortFile n tone oct = do
  datum <- readFile "./covid_cdna.txt"
  let dna = concat.words $ datum
  let peptide = (!! n) $ extractPeptides dna
  let sound = U.concat $ map (tone (scale peptide) 2 oct) $ peptideToSound peptide
  putWAVEFile ("tracks/temp"++show n++".wav") $ pack $ lowPass 1500 sound
