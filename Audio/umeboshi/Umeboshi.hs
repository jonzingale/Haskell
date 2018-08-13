module Umeboshi where
import qualified Data.Vector.Unboxed as U
import Sequencer
import Samples
import Wave
{--
Todo:
cymbals hang over vectorized measure (length sample > length subDiv)
display score
--}

testBuild = do
  [clHiHat, claves, cowbell, conga, crashCym, handClap, hiConga, 
   hiTom, kick, kick2, maracas, opHiHat, rimshot, snare, tom] <- roland808

  let m1 = [(".xx", hiTom),("xxxxx", maracas),(".", rimshot),
            ("x", opHiHat),(".x", handClap)]
  let m2 = [(".xx", hiTom),("xxxxxxx", maracas),("...x", snare),
            (".x..", rimshot),("x.", opHiHat),(".x", handClap)]
  let m3 = [(".", hiTom),(".x.", maracas),("x.x", rimshot),
            ("x.", opHiHat),(".x", handClap)]

  let track1 = buildTrack 120 (Time 5 4) m1                     
  let track2 = buildTrack 120 (Time 7 4) m2
  let track3 = buildTrack 120 (Time 3 4) m3

  let rhythm = [track2,track2,track1,track3,track1,track3,track3,track3,track3]
  makeWavFile $ pack $ U.toList $ U.concat $ rhythm
