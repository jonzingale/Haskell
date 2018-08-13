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

  let ensemble1 = [(".xx", hiTom),("xxxxx", maracas),
                   ("x", opHiHat),(".x", snare)]
  let ensemble2 = [(".xx", hiTom),("xxxxxxx", maracas),("...x", snare),
                   (".x..", rimshot),("x.", opHiHat),(".x", handClap)]
  let ensemble3 = [(".x.", maracas),("x.x", rimshot),
                   ("x.", opHiHat),(".x", handClap)]

  let m1 = buildMeasure 122 (Time 5 4) ensemble1
  let m2 = buildMeasure 122 (Time 7 4) ensemble2
  let m3 = buildMeasure 122 (Time 3 4) ensemble3

  let rhythm = [m2,m2,m2,
                m1,m3,m1,m3,
                m3,m3,m3]

  makeWavFile $ pack $ U.toList $ U.concat $ rhythm
