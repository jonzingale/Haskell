module SimpleSine where
import System.Random
import Data.WAVE
import Data.Int (Int32)
import Data.List.Split (splitOn)

type Frequency = Double
type SamplesPerSec = Int
type DurationSecs = Double
type Sample = [Int32]
type Volume = Int32

samplesPS = 16000
bitrate = 32

header = WAVEHeader 1 samplesPS bitrate Nothing

sound :: Frequency -> SamplesPerSec -> DurationSecs -> Volume -> [Int32]
sound freq samples len volume = take (round $ len * (fromIntegral samples)) $ 
   map (round . (* fromIntegral volume)) $
   map tan [0.0, (freq * 2 * pi / (fromIntegral samples))..]

samples :: [[Int32]]
samples = map (:[]) $ sound 600 samplesPS 3 (maxBound `div` 2)

samples2 :: [[Int32]] -- play two tones at once
samples2 = map (:[]) $ zipWith (+) (sound 1200 samplesPS 3 (maxBound `div` 2))
  (sound 10000 samplesPS 3 (maxBound `div` 2))

waveData = WAVE header samples2

makeWavFile :: WAVE -> IO ()
makeWavFile wav = putWAVEFile "temp.wav" wav

main = makeWavFile waveData

noise :: [Sample]
noise = map (:[]) $ take 48000 randos
  where randos = randoms $ mkStdGen 23