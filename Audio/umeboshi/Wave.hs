module Wave where
import Data.Int (Int32)
import Data.WAVE

type Sample = IO WAVE

header = WAVEHeader 1 44100 16 Nothing

unpack :: WAVE -> [Int32]
unpack = (map head).waveSamples

pack :: [Int32] -> WAVE
pack xs = WAVE header $ map (:[]) xs

makeWavFile :: WAVE -> IO ()
makeWavFile wav = putWAVEFile "temp.wav" wav
