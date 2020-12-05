module Main where
import DLAVector (bsize, blinkStates, board, bounds)
-- import Control.Parallel.Strategies (rdeepseq, parListChunk, using)
import qualified Data.Vector.Unboxed as U
import qualified Codec.Picture.Types as M
import Codec.Picture -- JuicyPixel
import Control.Monad.State
import Control.Monad.ST
import System.Random

-- . GenerateDLA.sh

-- For testing DLAVector
-- main = do -- 7000 steps, 4000 particles, 400x400 in 40 secs
--   let dla = runState (blinkStates 7000 board) (mkStdGen 12)
--   print dla

main :: IO ()
main = do savePngImage "images/tmp.png" $ ImageRGB8 genImage

genImage :: Image PixelRGB8
genImage = runST $ do
  let dla = runState (blinkStates 9000 board) (mkStdGen 42)
  let points = U.toList.bounds.fst $ dla
  mimg <- M.newMutableImage bsize bsize
  dlaToImage points mimg
  where
    dlaToImage [] mi = M.unsafeFreezeImage mi
    dlaToImage ((x,y):ps) mi =
      do writePixel mi x y (PixelRGB8 128 128 128)
         dlaToImage ps mi

-- TODO: Animated Gif
-- writeGifAnimation :: FilePath -> GifDelay -> GifLooping -> [Image PixelRGB8] -> Either String (IO ())
-- let gif = writeGifAnimation "tmp.gif" 10 LoopingForever [img, lmg]