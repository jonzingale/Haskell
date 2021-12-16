module Main where
import System.IO.Unsafe (unsafePerformIO) -- mixing IO and ST
import qualified Codec.Picture.Types as M
import qualified Data.Vector.Unboxed as U
import System.Environment (getArgs)
import Codec.Picture -- JuicyPixel
import Data.Int (Int32) -- 2^31-1
import Control.Monad.ST
import Line (drawLine)
import LorenzWave(singleLorenz)
import Data.WAVE

{--
This module constructs phase space manifolds from Lorenz wave files.
--}
 
-- Constants & Types
type File = String
type Density = Int
bsize = 10000 :: Int -- image size
hsize = fromIntegral bsize / 2.1 :: Float
delay = 120 -- heuristically found for Lorenz
density = 2

main :: IO ()
main = do
  savePngImage "images/lorenz.png" $ ImageRGB8 genImage

genImage :: Image PixelRGB8
genImage = runST $ do
  mimg <- M.newMutableImage bsize bsize
  let wav = unsafePerformIO $ takensFromWave
  toImage wav mimg

  where
    toImage [] mi = M.unsafeFreezeImage mi
    toImage ((x,y):ps) mi =
      do writePixel mi x y (PixelRGB8 128 200 150) -- trace green
      -- do writePixel mi x y (PixelRGB8 255 255 255) -- white
         toImage ps mi

takensFromWave :: IO [(Int, Int)]
takensFromWave = do
  wav <- getWAVEFile "audio/lorenz.wav"
  let xs = preprocess wav
  let ys = drop delay xs
  let zs = drop delay ys
  let as = drop delay zs
  return $ drawLine density $ zip xs ys

preprocess :: WAVE -> [Int]
preprocess wav =
  let samples = map (fromIntegral.head) $ waveSamples wav in
  let maxV = maximum.map abs $ samples in -- inefficient
  map (rescale maxV) samples
  where
    rescale mv x = floor $ x * hsize/mv + hsize
