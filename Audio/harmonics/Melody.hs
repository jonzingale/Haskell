module Melody where
import Harmonics(
  noiseTimbreEven, noiseTimbreOdd, evenTimbre,
  oddTimbre, squareTimbre, nonSquareTimbre, sawTimbre)
import qualified Data.Vector.Unboxed as U
import Wave (makeStereoWavFile)
import Data.Int (Int32)
import Solar (solar)
import Types

mkSolar = toMelody "solar.wav" solar

toMelody filename mel = do
  let sol = solar ++ solar
  let s1 = U.concat $ map (toSound sawTimbre) sol
  let s2 = U.concat $ map (toSound evenTimbre) sol
  makeStereoWavFile filename s1 s2

toPitch :: Int -> Frequency
toPitch (-1) = 0.0 -- rest
toPitch int = 110.0 * freq (fromIntegral int)
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

toSound :: Timbre -> Sound -> VectSamples
toSound timbre (note, duration) =
  let freq = fromNote note in
  let vol = maxBound `div` 2 :: Int32 in
  let setVol = U.map (round . (* fromIntegral vol)) in
  let noteTime = take.round $ toTime duration * 44100 in
  let harmonicSine = timbre freq :: [Double] in
  setVol $ U.fromList $ noteTime harmonicSine
