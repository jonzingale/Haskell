module Umeboshi where
import qualified Data.Vector.Unboxed as U
import Sequencer
import Samples
import Wave

-- lift time signatures and bpm
-- write mixInstruments.

testBuild = do
  [clHiHat,claves,cowbell,conga,crashCym,handClap,hiConga,
   hiTom,kick,kick2,maracas,opHiHat,rimshot,snare,tom] <- roland808

  let bank1 = buildTrack 120 (M (Time 5 4) ".xx") hiTom
  let bank2 = buildTrack 120 (M (Time 5 4) "xxxxx") maracas
  let bank3 = buildTrack 120 (M (Time 5 4) ".") rimshot
  let bank4 = buildTrack 120 (M (Time 5 4) "x") opHiHat
  let bank5 = buildTrack 120 (M (Time 5 4) ".x") handClap
  let drums1 = foldr (U.zipWith (+)) bank1 [bank2, bank3, bank4, bank5]

  let bank1 = buildTrack 120 (M (Time 7 4) ".xx") hiTom
  let bank2 = buildTrack 120 (M (Time 7 4) "xxxxxxx") maracas
  let bank6 = buildTrack 120 (M (Time 7 4) "...x") snare
  let bank3 = buildTrack 120 (M (Time 7 4) ".x..") rimshot
  let bank4 = buildTrack 120 (M (Time 7 4) "x.") opHiHat
  let bank5 = buildTrack 120 (M (Time 7 4) ".x") handClap
  let drums2 = foldr (U.zipWith (+)) bank1 [bank2, bank3, bank4, bank5, bank6]

  let bank1 = buildTrack 120 (M (Time 3 4) ".") hiTom
  let bank2 = buildTrack 120 (M (Time 3 4) ".x.") maracas
  let bank3 = buildTrack 120 (M (Time 3 4) "x.x") rimshot
  let bank4 = buildTrack 120 (M (Time 3 4) "x.") opHiHat
  let bank5 = buildTrack 120 (M (Time 3 4) ".x") handClap
  let drums3 = foldr (U.zipWith (+)) bank1 [bank2, bank3, bank4, bank5]

  let rhythm = [drums2,drums2,drums1,drums3,drums1,drums3,drums3,drums3,drums3]
  makeWavFile $ pack $ U.toList $ U.concat $ rhythm
