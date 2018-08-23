module Wave where
import qualified Data.Vector.Unboxed as U
import Filters.HiPass (hiPass, randos)
import Filters.BandPass (bandPass)
import Filters.LowPass (lowPass, fKernel, hh)
import Data.Int (Int32)
import Data.WAVE

type VectSamples = U.Vector Int32
type Frequency = Double
type SamplesPerSec = Int
type DurationSecs = Double
type Volume = Int32

header = WAVEHeader 1 44100 16 Nothing
header2 = WAVEHeader 2 44100 16 Nothing

-- unpack :: WAVE -> [Int32]
-- unpack = (map head).waveSamples

unpack :: WAVE -> VectSamples
unpack = (U.fromList).(map head).waveSamples

pack :: VectSamples -> WAVE
pack xs = WAVE header $ map (:[]) $ U.toList xs

makeWavFile :: VectSamples -> IO ()
makeWavFile wav = putWAVEFile "temp.wav" $ pack wav

makeStereoWavFile :: VectSamples -> VectSamples -> IO()
makeStereoWavFile w1 w2 = putWAVEFile "temp.wav" $ stereopack w1 w2

stereopack :: VectSamples -> VectSamples -> WAVE
stereopack xs ys =
  WAVE header2 $ mix (U.toList xs) (U.toList ys)
  where mix ls rs = [[a,b] | (a,b) <- zip ls rs]

-- viewSamples :: String -> []
-- viewSamples file = do
--   wav <- getWAVEFile file
--   print $ take 10 $ waveSamples wav

main = do
    w <- getWAVEFile "blow.wav"
    makeWavFile $ lowPass $ unpack w