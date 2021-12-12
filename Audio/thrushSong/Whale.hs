module Whale where
import System.IO.Unsafe (unsafePerformIO) -- mixing IO and ST
import qualified Data.Vector.Unboxed as U
import qualified Codec.Picture.Types as M
import Codec.Picture -- JuicyPixel
import Control.Monad.State
import Control.Monad.ST
import System.Random

import Lorenz
import LorenzWave (doubleLorenz, singleLorenz)

import Data.Int (Int32) -- 2^31-1
import Data.WAVE

{--
This module constructs phase space manifolds from wave files.
--}

-- Constants & Types
type File = String
bsize = 10000 :: Int -- image size
maxVal = (2^31-1) :: Int
hsize = fromIntegral $ div bsize 2
zsize = div bsize 2
delay = 120 -- heuristically found for Lorenz
time = 20

whale1 = "audio/whale.wav" -- humpback whale call
--

main :: IO ()
main = do savePngImage "images/tmp.png" $ ImageRGB8 genImage

genImage :: Image PixelRGB8
genImage = runST $ do
  mimg <- M.newMutableImage bsize bsize
  let wav = unsafePerformIO.takensFromWave $ whale1
  toImage wav mimg

  where
    toImage [] mi = M.unsafeFreezeImage mi
    toImage ((x,y):ps) mi =
      do writePixel mi x y (PixelRGB8 128 200 150)
         toImage ps mi

takensFromWave :: File -> IO [(Int, Int)]
takensFromWave file = do
  wav <- getWAVEFile file
  let xs = preprocess wav
  let ys = drop delay xs
  let zs = drop delay ys

  let as = drop delay zs
  return $ zip xs as
  where
    drop1 smps = drop (div (length smps) 3) smps
    take1 smps = take (div (length smps) 5) smps
    preprocess wav =
      map (rescale.fromIntegral.(!!0)).take1.drop1 $ waveSamples wav
    rescale x = zsize + div x (div maxVal zsize)
    xcoord (x,_,_) = x

