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

umeboshi = do
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

  let rhythm = U.concat [m2,m2,m2,
                         m1,m3,m1,m3,
                         m3,m3,m3]

  makeWavFile rhythm

standardNonStandard = do
  [clHiHat, claves, cowbell, conga, crashCym, handClap, hiConga,
   hiTom, kick, bass, maracas, opHiHat, rimshot, snare, tom] <- roland808

  let ensemble1 = [(".xx", hiTom),("xxxxxxxxxx", maracas),
                   ("x", opHiHat),(".x", snare),("x.", bass)]
  let ensemble2 = [("..x", hiTom),(".xxx.xxx", maracas),("...x", snare),
                   (".x..", rimshot),(".x", opHiHat),("x...", bass)]
  let ensemble3 = [(".x..x.", maracas),("x.x", rimshot),
                   (".x", opHiHat),("x.", kick)]
  let stopBass = [("x.xx..", bass)]

  let m1 = buildMeasure 120 (Time 5 4) ensemble1
  let m2 = buildMeasure 120 (Time 4 4) ensemble2
  let m3 = buildMeasure 120 (Time 3 4) ensemble3
  let m4 = buildMeasure 120 (Time 6 4) stopBass

  let rhythm = U.concat [m2,m2,m2,m2,m1,m3,m1,m3,
                         m2,m2,m2,m2,m4,m4,
                         m2,m2,m2,m2,m1,m3,m1,m3,
                         m2,m2,m2,m2,m4,m4,
                         m2,m2,m2,m2,m1,m3,m1,m3,
                         m2,m2,m2,m2,m4,m4,
                         m2,m2,m2,m2,m1,m3,m1,m3,
                         m2,m2,m2,m2,m4,m4,
                         m2,m2,m2,m2,m1,m3,m1,m3,
                         m2,m2,m2,m2,m4,m4,m4,m4]
  makeWavFile rhythm