module Main where
import qualified Data.Vector.Unboxed as U
import Data.Int (Int32)
import SortsShuffles
import SampleVector
import Data.WAVE

-- maxBound for Int32 is 2147483647 => 2^31 - 1
samplesPS = 44100
bitrate = 16

header = WAVEHeader 1 samplesPS bitrate Nothing

makeWavFile :: WAVE -> IO ()
makeWavFile wav = putWAVEFile "temp.wav" wav
{--
ghc -O2 --make WaveTest.hs
time ./WaveTest
rm *.hi *.o WaveTest
--}
main = do
  wav <- getWAVEFile "hello.wav"
  let stereo = waveSamples wav
  let mono = map (\ [x,y] -> x) stereo
  let keyed = zip [0..] mono
  let size = length mono

  let vv = U.fromList $ shuffle keyed
  let vals = take (2*10^6) $ iterativeSort size vv
  let samples = map (:[]) vals
  makeWavFile $ WAVE header samples
