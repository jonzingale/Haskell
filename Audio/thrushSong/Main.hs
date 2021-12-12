module Main where
import System.IO.Unsafe (unsafePerformIO) -- mixing IO and ST
import qualified Data.Vector.Unboxed as U
import qualified Codec.Picture.Types as M
import Codec.Picture -- JuicyPixel
import Control.Monad.State
import Control.Monad.ST
import System.Random

import Lorenz
import LorenzWave (doubleLorenz, singleLorenz)
import Wave

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

thrush = "HermitThrush.wav" -- monophonic
--

main :: IO ()
main = do savePngImage "images/tmp.png" $ ImageRGB8 genImage

lorenzXZ :: [(Int, Int)]
lorenzXZ =
  map (discrete.prj) $ runLorenz time (1,1,1)
  where
    discrete (x, y) = (floor x, floor y)
    prj (x,y,z) = (hsize + x * 200, hsize + z * 100)

takensLorenz :: [(Int, Int)]
takensLorenz =
  let xs = map xcoord $ runLorenz time (1,1,1) in
  let ys = drop delay xs in
  let zs = drop delay ys in
  map (discrete.resize) $ zip xs zs
  where
    xcoord (x,_,_) = x
    resize (x, y) = (hsize + x * 200, hsize + y * 100)
    discrete (x, y) = (floor x, floor y)

genImage :: Image PixelRGB8
genImage = runST $ do
  mimg <- M.newMutableImage bsize bsize

  -- original Lorenz
  -- toImage lorenzXZ mimg

  -- Takens reconstructed Lorenz
  -- toImage takensLorenz mimg

  -- thrush song
  let bird = unsafePerformIO takensFromWave
  toImage bird mimg

  where
    toImage [] mi = M.unsafeFreezeImage mi
    toImage ((x,y):ps) mi =
      do writePixel mi x y (PixelRGB8 128 200 150)
         toImage ps mi

-- prepares timeseries for display
preprocess :: File -> IO [Int]
preprocess file = do
  wav <- getWAVEFile file
  return $ map (rescale.fromIntegral.(!!0)) $ waveSamples wav
  where
    rescale x = zsize + div x (div maxVal zsize)

takensFromWave :: IO [(Int, Int)]
takensFromWave = do
  bird <- preprocess thrush
  -- bird <- preprocess "temp.wav" -- doubleLorenz to make this
  let ys = drop delay bird
  let zs = drop delay ys
  return $ zip bird zs
  where
    xcoord (x,_,_) = x
