module MixTrack where
import qualified Data.Vector.Unboxed as U
import Data.Int (Int32)
import Sequencer
import Samples
import Wave

type VectSamples = U.Vector Int32

{--
Todo:
cymbals hang over vectorized measure (length sample > length subDiv)
--}

-- mkEmptyMeasure 120 exM
mkEmptyZeroVector :: BPM -> Measure -> VectSamples
mkEmptyZeroVector bpm (M (Time b _) _) =
  let beats = (fromIntegral b)::Float in
  let samplesPerMeasure = beats * 60 * 44100 / bpm in
  U.replicate (ceiling samplesPerMeasure) (0::Int32)

buildTrack :: BPM -> Measure -> [Int32] -> VectSamples
buildTrack bpm (M (Time n m) mstr) samples =
  let empty = mkEmptyZeroVector bpm (M (Time n m) mstr) in
  let subDiv = div (U.length empty) (length mstr) in
  (U.//) empty (f mstr 0 subDiv samples)
  -- U.accum (+) empty (f mstr 0 subDiv samples)
  where
    f [] _ _ _ = []
    f (x:xs) i subDiv ss
      | x == 'x' = getPairs ss i subDiv ++ f xs (i+1) subDiv ss
      | otherwise = f xs (i+1) subDiv ss

    getPairs ss i subDiv = 
      [(j + i*subDiv, val) | (j, val) <- zip [0..] ss]

testBuild = do
  w1 <- hiTom
  w2 <- tom
  w3 <- rimshot
  w4 <- opHiHat
  w5 <- handClap
  let [drum1, drum2, drum3, drum4, drum5] = map unpack [w1, w2, w3, w4, w5]
  -- let track1 = buildTrack 120 (M (Time 5 4) ".xx") drum1
  -- let track2 = buildTrack 120 (M (Time 5 4) ".xxxx") drum2
  -- let track3 = buildTrack 120 (M (Time 5 4) ".") drum3
  -- let track4 = buildTrack 120 (M (Time 5 4) "x") drum4
  -- let track5 = buildTrack 120 (M (Time 5 4) ".x") drum5

  let track1 = buildTrack 120 (M (Time 7 4) ".xx") drum1
  let track2 = buildTrack 120 (M (Time 7 4) ".xxxxxx") drum2
  let track3 = buildTrack 120 (M (Time 7 4) ".x.x") drum3
  let track4 = buildTrack 120 (M (Time 7 4) "x") drum4
  let track5 = buildTrack 120 (M (Time 7 4) ".x") drum5

  let track = foldr (U.zipWith normSum) track1 [track2, track3, track4, track5]

  makeWavFile $ pack $ U.toList $ U.concat [track,track,track]

normSum :: Int32 -> Int32 -> Int32
normSum s t =
  let s' = fromIntegral s / (2**31-1) in
  let t' = fromIntegral t / (2**31-1) in
  ceiling((s' + t') * (2**31-1))
