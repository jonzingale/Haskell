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
bsize = 3000 :: Int -- image size
hsize = fromIntegral bsize / 2.1 :: Float
delay = 120 -- heuristically found for Lorenz
density = 20

-- wav1 = "audio/peptideSymphony.wav" -- codon
-- wav1 = "audio/umeboshi1.wav"
-- wav1 = "audio/whistle.wav"
-- wav1 = "audio/umeboshi_with_mobius.wav"
-- wav1 = "audio/rebab.wav"
-- wav1 = "audio/whale.wav"
-- wav1 = "audio/HermitThrush.wav"

thrush = main "audio/HermitThrush.wav"

main :: File -> IO ()
main file = do savePngImage "images/tmp.png" $ ImageRGB8 (genImage file)

genImage :: File -> Image PixelRGB8
genImage file = runST $ do
  mimg <- M.newMutableImage bsize bsize
  let wav = unsafePerformIO.takensFromWave $ file
  toImage wav mimg

  where
    toImage [] mi = M.unsafeFreezeImage mi
    toImage ((x,y):ps) mi =
      do writePixel mi x y (PixelRGB8 128 200 150) -- trace green
      -- do writePixel mi x y (PixelRGB8 255 255 255) -- white
         toImage ps mi

takensFromWave :: File -> IO [(Int, Int)]
takensFromWave file = do
  wav <- getWAVEFile file
  let xs = preprocess wav
  let ys = drop delay xs
  let zs = drop delay ys
  let as = drop delay zs
  return $ drawLine density $ zip xs ys

preprocess :: WAVE -> [Int]
preprocess wav =
  let samples = map head $ waveSamples wav in
  let maxV = fromIntegral.maximum.map abs $ samples in -- inefficient
  map ((rescale maxV).fromIntegral) samples
  where
    rescale mv x = floor $ fromIntegral x * hsize/mv + hsize
