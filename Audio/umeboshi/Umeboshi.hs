module Umeboshi where
import qualified Data.Vector.Unboxed as U
import Data.Int (Int32)
import Data.WAVE
import Samples

data Rhythm = Rhythm WAVE String
data Track = Track Rhythm

instance Show Rhythm where
  show (Rhythm wav str) = str

-- measure based "x.x.x" => 5/4
-- sampleToString :: Sample -> String -> 

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
  wav <- getWAVEFile "./808Soundz/cl_hihat.wav"
  let mono' = waveSamples wav
  let mono = map (\ [x] -> x) mono'

  let them = foldr (++) [] $ take 20 $ repeat mono
  -- let keyed = zip [0..] mono
  -- let size = length mono

  -- let vv = U.fromList $ shuffle keyed
  -- let vals = take (2*10^6) $ iterativeSort size vv
  let samples = map (:[]) them
  makeWavFile $ WAVE header samples