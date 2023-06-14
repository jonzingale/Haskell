module Main where
import Codec.EDF (getSignalsFromFile, Label, Samples)
import System.IO.Unsafe (unsafePerformIO) -- mixing IO and ST
import qualified Codec.Picture.Types as M
import System.Environment (getArgs)
import Codec.Picture -- JuicyPixel
import Data.Int (Int32) -- 2^31-1
import Control.Monad.ST
import Line (drawLine)

{--
This module constructs phase space manifolds from edf files.
edf parsing library found https://github.com/enomsg/edf

usage:
. compile.sh ; time ./Main "edf/dX.edf" 10 ; open "images/tmp.png"

https://physionet.org/content/nifecgdb/1.0.0/
--}
 
-- Constants & Types
type File = String
type Density = Int
bsize = 10000 :: Int -- image size
hsize = fromIntegral bsize / 2.1 :: Float
rsize = fromIntegral bsize / 1.1 :: Float
delay = 120 -- 80 -- 120 heuristically found for Lorenz

main :: IO ()
main = do
  args <- getArgs
  let [file, density] = args
  savePngImage "images/tmp.png" $ ImageRGB8 (genImage file (read density))

-- prints three cross sections
genImage :: File -> Density -> Image PixelRGB8
genImage file density = runST $ do
  mimg <- M.newMutableImage bsize bsize
  let sig = unsafePerformIO.takensFromEdf file $ density
  toImage sig mimg

  where
    toImage [] mi = M.unsafeFreezeImage mi
    toImage ((x,y):ps) mi =
      do writePixel mi x y (PixelRGB8 128 200 150) -- trace green
      -- do writePixel mi x y (PixelRGB8 255 255 255) -- white
         toImage ps mi

takensFromEdf :: File -> Density -> IO [(Int, Int)]
takensFromEdf file density = do
  signals <- getSignalsFromFile file
  let xs = preprocess signals
  let ys = drop delay xs
  let zs = drop delay ys
  let as = drop delay zs
  return $ drawLine density $ zip xs as

preprocess :: [(Codec.EDF.Label, Codec.EDF.Samples)] -> [Int]
preprocess signals =
  let samples = (map fromIntegral).snd.head $ signals in
  let maxV = maximum.map abs $ samples in
  let minV = minimum samples in
  map (rescale maxV minV) samples
  where
    rescale maxV minV x
      | minV >= 0 = floor $ x * rsize/maxV
      | otherwise = floor $ x * hsize/maxV + hsize
