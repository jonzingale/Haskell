module Main where
import qualified Data.Vector.Unboxed as U
import Data.Int (Int32)
import System.Random
import SortsShuffles
import SampleVector
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
ghc -O2 --make WaveTest.hs
time ./WaveTest
rm *.hi *.o WaveTest
--}
main = do
  wav <- getWAVEFile "hello.wav"
  let size = 16000
  let stereo = waveSamples wav
  let mono = map (\ [x,y] -> x) stereo
  let keyed = zip [0..] $ take size mono
  let vv = U.fromList $ shuffle keyed

  let vals = take (1*10^6) $ iterativeSort size vv
  let samples = map (:[]) vals
  makeWavFile $ WAVE header samples
