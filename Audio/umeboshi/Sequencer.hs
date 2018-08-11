module Sequencer where
import Data.Int (Int32)
import Data.WAVE
import Samples
import Wave

{--
Parser?
"x..x..x.."

"x........"
"...x....."
"......x.."
then sums
--}

type BPM = Float
type SamplesPerBeat = Float
data Signature = Time Int Int -- example: 3 4
data Measure = M Signature String
data Track = Trk Sample [Measure]

instance Show Measure where
  show (M (Time b q) str) = show b ++ "/" ++ show q ++ ": " ++ str

toMeasure :: Signature -> String -> Measure
toMeasure sig str = M sig str

exM :: Measure
exM = toMeasure (Time 3 4) "x.x.xx."

exMix = trackToWave 12 (Trk snare [M (Time 3 4) "x.."])

trackToWave :: BPM -> Track -> IO()
trackToWave bpm (Trk wavIO ms) = do
  wav <- wavIO
  let mono = unpack wav
  let emptyTrack = foldr (++) [] $ map (mkEmptyMeasure bpm) ms -- expensive
  let them = mixA mono emptyTrack
  makeWavFile $ pack them

-- Helpers
sampsPerBeat :: BPM -> SamplesPerBeat
sampsPerBeat bpm = 60 * 44100 / bpm

mkEmptyMeasure :: BPM -> Measure -> [Int32]
mkEmptyMeasure bpm (M (Time b _) _) =
  let beats = (fromIntegral b)::Float in
  let sampPerMeasure = bpm * beats * 735 in
  take (floor sampPerMeasure) $ repeat (0::Int32)

mixA :: [Int32] -> [Int32] -> [Int32]
mixA as bs = zipWith (+) as bs -- mix mono mono
