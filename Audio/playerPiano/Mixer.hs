module Mixer where
import qualified Data.Vector.Unboxed as U
import Data.WAVE
import Wave

type WavFile = String

-- TODO: Needs non truncation protections
mixAudio :: WavFile -> WavFile -> IO()
mixAudio f1 f2 = do
  w1 <- getWAVEFile f1
  w2 <- getWAVEFile f2
  let sl = U.fromList $ map (!! 0) $ waveSamples w1
  -- let sr = U.fromList $ map (!! 1) $ waveSamples w1

  let tl = U.fromList $ map (!! 0) $ waveSamples w2
  let tr = U.fromList $ map (!! 1) $ waveSamples w2

  let left  = U.zipWith (+) sl tl
  let right = U.zipWith (+) sl tr

  makeStereoWavFile left right

  -- print ""
  -- let [ss, ts] = map stereoUnpack [w1, w2]
  -- makeStereoWavFile ss tt



