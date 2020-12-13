module Main where
import qualified Data.Vector.Unboxed as U
import Harmonics(
  noiseTimbreEven, noiseTimbreOdd, evenTimbre, emptyTimbre,
  oddTimbre, squareTimbre, nonSquareTimbre, sawTimbre)
import LongTones (longTones1, longTones2, longTones3)
import Lorenz (trajX , trajY, trajZ)
import Wave (makeStereoWavFile)
import Data.Int (Int32)
import Types

{--
1. Melodies are generated from longtones.
2. harmonics are given to each sound.
3. sounds are convolved against a dynamical system.
--}

main = toMelody "longTones.wav" longTones1

toMelody filename melody = do
  let mel = melody ++ reverse melody ++ melody
  let sine = U.concat $ map (toSoundX emptyTimbre) mel
  let sqr = U.concat $ map (toSoundY squareTimbre) mel -- toSoundY
  let saw = U.concat $ map (toSoundZ sawTimbre) mel -- toSoundZ
  let se = U.concat $ map (toSoundX evenTimbre) mel -- toSoundX
  let so = U.concat $ map (toSoundX nonSquareTimbre) mel
  makeStereoWavFile filename (mix sqr saw) (mix saw se)
  -- makeStereoWavFile filename sine sine

mix :: VectSamples -> VectSamples -> VectSamples
mix s1 s2 = U.map (flip div 2) $ U.zipWith (+) s1 s2

-- duration a whole note expresses in seconds
stretch = 6

toSoundX :: Timbre -> Sound -> VectSamples
toSoundX timbre (note, duration) =
  let freq = fromNote note in
  let vol = maxBound `div` 2 :: Int32 in
  let setVol = U.map (round . (* fromIntegral vol)) in
  let noteTime = take.round $ toTime duration * 44100 * stretch in
  let harmonicSine = timbre freq :: [Double] in
  let convolve = zipWith (*) trajX in -- TESTING: convolutions with lorenz
  setVol $ U.fromList $ convolve.noteTime $ harmonicSine

toSoundY :: Timbre -> Sound -> VectSamples
toSoundY timbre (note, duration) =
  let freq = fromNote note in
  let vol = maxBound `div` 2 :: Int32 in
  let setVol = U.map (round . (* fromIntegral vol)) in
  let noteTime = take.round $ toTime duration * 44100 * stretch in
  let harmonicSine = timbre freq :: [Double] in
  let convolve = zipWith (*) trajY in -- TESTING: convolutions with lorenz
  setVol $ U.fromList $ convolve.noteTime $ harmonicSine

toSoundZ :: Timbre -> Sound -> VectSamples
toSoundZ timbre (note, duration) =
  let freq = fromNote note in
  let vol = maxBound `div` 2 :: Int32 in
  let setVol = U.map (round . (* fromIntegral vol)) in
  let noteTime = take.round $ toTime duration * 44100 * stretch in
  let harmonicSine = timbre freq :: [Double] in
  let convolve = zipWith (*) trajZ in -- TESTING: convolutions with lorenz
  setVol $ U.fromList $ convolve.noteTime $ harmonicSine

toPitch :: Int -> Frequency
toPitch (-1) = 0.0 -- rest
toPitch int = 70 * freq (fromIntegral int)
  where freq = \n -> 2.0 ** (n/12)

fromNote :: String -> Frequency
fromNote = toPitch.note2Iint

note2Iint :: String -> Int
note2Iint interval =
  case interval of
    "r" -> -1
    "c0" -> 0
    "c'0" -> 1
    "d0" -> 2
    "d'0" -> 3
    "e0" -> 4
    "f0" -> 5
    "f'0" -> 6
    "g0" -> 7
    "g'0" -> 8
    "a0" -> 9
    "a'0" -> 10
    "b0" -> 11
    "c1" -> 12
    "c'1" -> 13
    "d1" -> 14
    "d'1" -> 15
    "e1" -> 16
    "f1" -> 17
    "f'1" -> 18
    "g1" -> 19
    "g'1" -> 20
    "a1" -> 21
    "a'1" -> 22
    "b1" -> 23
    "c2" -> 24
    "c'2" -> 25
    "d2" -> 26
    "d'2" -> 27
    "e2" -> 28
    "f2" -> 29
    "f'2" -> 30
    "g2" -> 31
    "g'2" -> 32
    "a2" -> 33
    "a'2" -> 34
    "b2" -> 35


toTime :: Duration -> DurationSecs
toTime Eighth = 0.125
toTime EighthD = 0.1875
toTime Quarter = 0.25
toTime QuarterD = 0.375
toTime Half = 0.5
toTime HalfD = 0.75
toTime Whole = 1
toTime WholeD = 1.5
