module Sequencer where
import qualified Data.Vector.Unboxed as U
import Data.Int (Int32)
import Data.WAVE
import Wave

type Performance = (Rhythm, WAVE)
data Signature = Time Int Int
type Rhythm = String
type BPM = Float

buildMeasure :: BPM -> Signature -> [Performance] -> VectSamples
buildMeasure bpm (Time n m) (sw:sws) =
  let empty = mkEmptyMeasure bpm (Time n m) in
  let banks = map (mixinBank empty) (sw:sws) in
  U.accum (+) empty $ concat banks

mkEmptyMeasure :: BPM -> Signature -> VectSamples
mkEmptyMeasure bpm (Time b _) =
  let beats = (fromIntegral b)::Float in
  let samplesPerMeasure = beats * 60 * 44100 / bpm in
  U.replicate (ceiling samplesPerMeasure) (0::Int32)

-- TODO: the use of the token empty here is (mis)leading.
-- mixinBank takes allocated space and mixes in a performance.
mixinBank :: VectSamples -> Performance -> [(Int, Int32)]
mixinBank empty (mstr, ss) =
  let subDiv = div (U.length empty) (length mstr) in
  f mstr 0 subDiv (unpack ss)
  where
    f [] _ _ _ = []
    f (x:xs) i d ss
      | x == 'x' = getPairs ss i d ++ f xs (i+1) d ss
      | otherwise = f xs (i+1) d ss

    getPairs ss i d = [(j + i*d, div v 5) | (j, v) <- zip [0..] ss]
