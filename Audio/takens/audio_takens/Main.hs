module Main where
import System.IO.Unsafe (unsafePerformIO) -- mixing IO and ST
import qualified Codec.Picture.Types as M
import System.Environment (getArgs)
import Codec.Picture -- JuicyPixel
import Data.Int (Int32) -- 2^31-1
import Control.Monad.ST
import Line (drawLine)
import Data.WAVE

{--
This module constructs phase space manifolds from wave files.

usage:
. compile.sh ; time ./Main "audio/peptideSymphony.wav" 1

time ./Main "audio/HermitThrush.wav" 40
time ./Main "audio/peptideSymphony.wav" 1
time ./Main "audio/umeboshi1.wav" 2
time ./Main "audio/whistle.wav" 20
time ./Main "audio/rebab.wav" 30
time ./Main "audio/whale.wav" 20
--}
 
-- Constants & Types
type File = String
type Density = Int
bsize = 10000 :: Int -- image size
hsize = fromIntegral bsize / 2.1 :: Float
delay = 80 -- 120 heuristically found for Lorenz

main :: IO ()
main = do
  args <- getArgs
  let [file, density] = args
  savePngImage "images/tmp.png" $ ImageRGB8 (genImage file (read density))

genImage :: File -> Density -> Image PixelRGB8
genImage file density = runST $ do
  mimg <- M.newMutableImage bsize bsize
  let wav = unsafePerformIO.takensFromWave file $ density
  toImage wav mimg

  where
    toImage [] mi = M.unsafeFreezeImage mi
    toImage ((x,y):ps) mi =
      do writePixel mi x y (PixelRGB8 128 200 150) -- trace green
      -- do writePixel mi x y (PixelRGB8 255 255 255) -- white
         toImage ps mi

takensFromWave :: File -> Density -> IO [(Int, Int)]
takensFromWave file density = do
  wav <- getWAVEFile file
  let xs = preprocess wav
  let ys = drop delay xs
  let zs = drop delay ys
  let as = drop delay zs
  return $ drawLine density $ zip xs zs

preprocess :: WAVE -> [Int]
preprocess wav =
  let samples = map (fromIntegral.head) $ waveSamples wav in
  let maxV = maximum.map abs $ samples in -- inefficient
  map (rescale maxV) samples
  where
    rescale mv x = floor $ x * hsize/mv + hsize
