module AudioTakens where
import System.IO.Unsafe (unsafePerformIO) -- mixing IO and ST
import qualified Data.Vector.Unboxed as U
import qualified Codec.Picture.Types as M
import Codec.Picture -- JuicyPixel
import Data.Int (Int32) -- 2^31-1
import Control.Monad.State
import Control.Monad.ST
import System.Random
import Data.WAVE

import Line (drawLine)

{--
This module constructs phase space manifolds from wave files.
--}

-- Constants & Types
type File = String
bsize = 5000 :: Int -- image size
maxVal = 2^31-1 :: Int
hsize = div bsize 2
delay = 120 -- heuristically found for Lorenz
density = 10

-- wav1 = "audio/peptideSymphony.wav" -- codon
wav1 = "audio/umeboshi1.wav"
-- wav1 = "audio/whistle.wav"
-- wav1 = "audio/umeboshi_with_mobius.wav"
-- wav1 = "audio/rebab.wav"
-- wav1 = "audio/whale.wav"
--

main :: IO ()
main = do savePngImage "images/tmp.png" $ ImageRGB8 genImage

genImage :: Image PixelRGB8
genImage = runST $ do
  mimg <- M.newMutableImage bsize bsize
  let wav = unsafePerformIO.takensFromWave $ wav1
  toImage wav mimg

  where
    toImage [] mi = M.unsafeFreezeImage mi
    toImage ((x,y):ps) mi =
      -- do writePixel mi x y (PixelRGB8 128 200 150)
      do writePixel mi x y (PixelRGB8 255 255 255)
         toImage ps mi

takensFromWave :: File -> IO [(Int, Int)]
takensFromWave file = do
  wav <- getWAVEFile file
  let xs = preprocess wav
  let ys = drop delay xs
  let zs = drop delay ys

  let as = drop delay zs
  return $ drawLine density $ zip xs ys
  where
    preprocess wav = map (rescale.fromIntegral.(!!0)) $ waveSamples wav
    rescale x = hsize + div x (div maxVal hsize)
    xcoord (x,_,_) = x

