module Umeboshi where
import qualified Data.Vector.Unboxed as U
import Sequencer
import Samples
import Wave

testBuild = do
  w1 <- hiTom
  w2 <- maracas
  w3 <- rimshot
  w4 <- opHiHat
  w5 <- handClap
  let [drum1, drum2, drum3, drum4, drum5] = map unpack [w1, w2, w3, w4, w5]
  let track1 = buildTrack 130 (M (Time 5 4) ".xx") drum1
  let track2 = buildTrack 130 (M (Time 5 4) "xxxxx") drum2
  let track3 = buildTrack 130 (M (Time 5 4) ".") drum3
  let track4 = buildTrack 130 (M (Time 5 4) "x") drum4
  let track5 = buildTrack 130 (M (Time 5 4) ".x") drum5
  let drums1 = foldr (U.zipWith (+)) track1 [track2, track3, track4, track5]

  let track1 = buildTrack 130 (M (Time 7 4) ".xx") drum1
  let track2 = buildTrack 130 (M (Time 7 4) "xxxxxxx") drum2
  let track3 = buildTrack 130 (M (Time 7 4) ".x.x") drum3
  let track4 = buildTrack 130 (M (Time 7 4) "x.") drum4
  let track5 = buildTrack 130 (M (Time 7 4) ".x") drum5
  let drums2 = foldr (U.zipWith (+)) track1 [track2, track3, track4, track5]

  let track1 = buildTrack 130 (M (Time 3 4) ".") drum1
  let track2 = buildTrack 130 (M (Time 3 4) ".x.") drum2
  let track3 = buildTrack 130 (M (Time 3 4) "x.x") drum3
  let track4 = buildTrack 130 (M (Time 3 4) "x.") drum4
  let track5 = buildTrack 130 (M (Time 3 4) ".x") drum5
  let drums3 = foldr (U.zipWith (+)) track1 [track2, track3, track4, track5]
  makeWavFile $ pack $ U.toList $ U.concat [drums1,drums2,drums1,drums3]
