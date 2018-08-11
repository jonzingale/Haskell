module MixTrack where
import qualified Data.Vector.Unboxed as U
import Data.Int (Int32)
import Sequencer
import Samples
import Wave

type VectSamples = U.Vector Int32

-- mkEmptyMeasure 120 exM
mkEmptyZeroVector :: BPM -> Measure -> VectSamples
mkEmptyZeroVector bpm (M (Time b _) _) =
  let beats = (fromIntegral b)::Float in
  let samplesPerMeasure = bpm * beats * 735 in
  U.replicate (floor samplesPerMeasure) (0::Int32)

buildTrack :: BPM -> Measure -> [Int32] -> VectSamples
buildTrack bpm (M (Time n m) mstr) samples =
  let empty = mkEmptyZeroVector bpm (M (Time n m) mstr) in
  let subDiv = div (U.length empty) (length mstr) in
  (U.//) empty (f bpm n mstr 1 subDiv samples)
  where
    f _ _ [] _ _ _ = []
    f bpm n (x:xs) i subDiv ss
      | x == 'x' = getPairs ss i subDiv ++ (f bpm n xs (i+1) subDiv ss)
      | otherwise = f bpm n xs (i+1) subDiv ss

    getPairs ss i subDiv = 
      [ (j + i*subDiv, val) | (j, val) <- zip [0..] ss]

testBuild = do
  wav <- maracas
  let drum = unpack wav
  let track = buildTrack 60 (M (Time 3 4) "x..xx.x..") drum
  makeWavFile $ pack $ U.toList track