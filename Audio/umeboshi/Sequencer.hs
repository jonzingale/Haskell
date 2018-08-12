module Sequencer where
import qualified Data.Vector.Unboxed as U
import Data.Int (Int32)
import Data.WAVE
import Wave

type BPM = Float
data Signature = Time Int Int -- example: 3 4
data Measure = M Signature String
type VectSamples = U.Vector Int32

instance Show Measure where
  show (M (Time b q) str) = show b ++ "/" ++ show q ++ ": " ++ str

{--
Todo:
cymbals hang over vectorized measure (length sample > length subDiv)
dithering in mkEmptyZeroVector
display score

rewrite build track to externalize emptyVector
and as buildTracks without replicating BPM and TimeSig in same measure.
--}

-- mkEmptyMeasure 120 exM
mkEmptyZeroVector :: BPM -> Measure -> VectSamples
mkEmptyZeroVector bpm (M (Time b _) _) =
  let beats = (fromIntegral b)::Float in
  let samplesPerMeasure = beats * 60 * 44100 / bpm in
  U.replicate (ceiling samplesPerMeasure) (0::Int32) -- dither this?

buildTrack :: BPM -> Measure -> WAVE -> VectSamples
buildTrack bpm (M (Time n m) mstr) ss =
  let samples = unpack ss in
  let empty = mkEmptyZeroVector bpm (M (Time n m) mstr) in
  let subDiv = div (U.length empty) (length mstr) in
  U.accum (+) empty (f mstr 0 subDiv samples)
  where
    f [] _ _ _ = []
    f (x:xs) i subDiv ss
      | x == 'x' = getPairs ss i subDiv ++ f xs (i+1) subDiv ss
      | otherwise = f xs (i+1) subDiv ss

    getPairs ss i subDiv = 
      [(j + i*subDiv, val) | (j, val) <- zip [0..] ss]
