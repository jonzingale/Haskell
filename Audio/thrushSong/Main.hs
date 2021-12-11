module Main where
import qualified Data.Vector.Unboxed as U
import qualified Codec.Picture.Types as M
import Codec.Picture -- JuicyPixel
import Control.Monad.State
import Control.Monad.ST
import System.Random

import Lorenz
import LorenzWave (doubleLorenz, singleLorenz)
import Wave

import Data.Int (Int32) -- 2,147,483,647
import Data.WAVE

import System.IO.Unsafe (unsafePerformIO) -- mixing IO and ST

{--
Todo:
Make a Takens visualizer for bird song.
- write autoscaling based on params: max, min
- principle components of syrinx?

- Tomorrow:
1. Scale Int32 better to the page!!!
2. Test will be to produce timeseries of lorenz and validate.
Check one and combined two dimensional inputs
--}

-- Constants & Types
type File = String
bsize = 10000 :: Int -- image size
maxVal = (2^31-1) :: Int
hsize = fromIntegral $ div bsize 2
zsize = div bsize 2
delay = 120 -- heuristically found for Lorenz
-- delay = 12 -- heuristically found.
time = 20

thrush = "HermitThrush.wav" -- monophonic
random = "random.wav"
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

-- genImage :: Image PixelRGB8
genImage =
  runST $ do
  mimg <- M.newMutableImage bsize bsize

  -- original Lorenz
  -- toImage lorenzXZ mimg

  -- Takens reconstructed Lorenz
  -- toImage takensLorenz mimg

  -- thrush song
  let bird = (unsafePerformIO $ takensThrush)
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
  return $ map (rescale.toInt.(!!0)) $ waveSamples wav
  where
    rescale x = zsize + div x (div maxVal 5000)
    toInt x = (fromIntegral x)::Int

takensThrush :: IO [(Int, Int)]
takensThrush = do
  -- bird <- preprocess thrush
  bird <- preprocess "temp.wav"
  let ys = drop delay bird
  let zs = drop delay ys
  return $ zip bird zs
  where
    xcoord (x,_,_) = x
