module Samples where
import Data.WAVE

roland808 = do
  clHiHat  <- getWAVEFile "./808Soundz/cl_hihat.wav"
  claves   <- getWAVEFile "./808Soundz/claves.wav"
  cowbell  <- getWAVEFile "./808Soundz/cowbell.wav"
  conga    <- getWAVEFile "./808Soundz/conga1.wav"
  crashCym <- getWAVEFile "./808Soundz/crashcym.wav"
  handClap <- getWAVEFile "./808Soundz/handclap.wav"
  hiConga  <- getWAVEFile "./808Soundz/hi_conga.wav"
  hiTom    <- getWAVEFile "./808Soundz/hightom.wav"
  kick     <- getWAVEFile "./808Soundz/kick1.wav"
  kick2    <- getWAVEFile "./808Soundz/kick2.wav"
  maracas  <- getWAVEFile "./808Soundz/maracas.wav"
  opHiHat  <- getWAVEFile "./808Soundz/open_hh.wav"
  rimshot  <- getWAVEFile "./808Soundz/rimshot.wav"
  snare    <- getWAVEFile "./808Soundz/snare.wav"
  tom      <- getWAVEFile "./808Soundz/tom1.wav"

  return [clHiHat, claves, cowbell, conga, crashCym, handClap, hiConga,
          hiTom, kick, kick2, maracas, opHiHat, rimshot, snare, tom]
