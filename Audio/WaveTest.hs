module Main where
import Control.Parallel.Strategies (rdeepseq, parListChunk, rseq, using)
import Data.Int (Int32)
import System.Random
import BubbleSort
import Data.WAVE

-- maxBound for Int32 is 2147483647 => 2^31 - 1
samplesPS = 16000
bitrate = 32

header = WAVEHeader 1 samplesPS bitrate Nothing

type Frequency = Double
type SamplesPerSec = Int
type DurationSecs = Double
type Sample = [Int32]
type Volume = Int32

makeWavFile :: WAVE -> IO ()
makeWavFile wav = putWAVEFile "temp.wav" wav

{--
ghc -O2 --make WaveTest.hs -threaded -rtsopts
time ./WaveTest +RTS -N8
rm *.hi *.o WaveTest
--}
main = do
  w <- getWAVEFile "test.wav"
  let stereo = take 16000 $ waveSamples w -- 16k == 1 sec
  let mono = map (\ [x,y] -> [x]) stereo
  let shKeyMono = shuffle $ toKeyedSamples mono
  let them = iterate (partialBubbleSort 1000) shKeyMono
  let sampChunks = take 300 $ map unkey them
  let pSamps = sampChunks `using` parListChunk 64 rdeepseq
  makeWavFile $ WAVE header $ foldr (++) [] pSamps
