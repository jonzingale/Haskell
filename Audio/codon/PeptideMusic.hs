module PeptideMusic where
import qualified Data.Vector.Unboxed as U
import AminoAcidToPitch (frequency)
import AminoAcidToPitch (Freq)
import Data.Int (Int32)
import Peptide
import Event
import Wave

type Sound = (Freq, Epoch, Duration)

ev :: Sound
ev = head notesDuration

-- 
freqPerSample :: Double -> Double
freqPerSample freq = freq * 2 * pi / 44100

toTime :: Duration -> DurationSecs
toTime Eighth = 0.125
toTime Quarter = 0.25
toTime Half = 0.5
toTime Whole = 1

soundToTime :: Epoch -> Duration -> (DurationSecs, DurationSecs)
soundToTime e d
  | e < d = (toTime e, toTime e)
  | otherwise = (toTime d, toTime e - toTime d)

toSound :: Sound -> VectSamples
toSound (freq, epoch, dur) =
  let vol = maxBound `div` 1 :: Int32 in
  let setVol = U.map (round . (* fromIntegral vol)) in
  let (eSec, dSec) = soundToTime epoch dur in
  let sine = map sin [0.0, freqPerSample freq..] in
  let noteTime = take.round $ eSec * 44100 in
  let restTime = take (round $ dSec * 44100) (repeat 0) in
  setVol $ U.fromList $ noteTime sine ++ restTime

peptideToSound :: Peptide -> [(Freq, Epoch, Duration)]
peptideToSound peptide = map toSound
  [(frequency.pitch $ c, epoch c, duration c) | c <- peptideToEvents peptide]

main = do
  datum <- readFile "covid_cdna.txt"
  let dna = concat.words $ datum
  let peptide = (!! 0) $ extractPeptides dna
  let sound = peptideToSound peptide
  makeWavFile $ U.concat sound



