module Main where
-- import DLAVector (blinkStates, board, bounds)
import qualified Data.Vector.Unboxed as U
import qualified Codec.Picture.Types as M
import Codec.Picture -- JuicyPixel
import Control.Monad.State
import Control.Monad.ST
import System.Random

import Lorenz
import Wave

{--
Todo:
Make a Takens visualizer for bird song.
- write autoscaling based on params: max, min
- principle components of syrinx?
--}

bsize = 10000 :: Int -- board size
hsize = fromIntegral $ div bsize 2

main :: IO ()
main = do savePngImage "images/tmp.png" $ ImageRGB8 genImage

points :: [(Int, Int)]
points = [ (a, a) | a <- [1..bsize-1] ]

lorenzXZ :: [(Int, Int)]
lorenzXZ =
  map (discrete.prj) $ runLorenz 60 (1,1,1)
  where
    discrete = (\(x,y) -> (floor x, floor y))
    prj = (\(x,y,z) -> (hsize + x * 200, hsize/2 + z * 100))

genImage :: Image PixelRGB8
genImage = runST $ do
  -- let dla = runState (blinkStates blinks board) (mkStdGen 42)
  mimg <- M.newMutableImage bsize bsize
  dlaToImage lorenzXZ mimg
  where
    dlaToImage [] mi = M.unsafeFreezeImage mi
    dlaToImage ((x,y):ps) mi =
      do writePixel mi x y (PixelRGB8 128 200 150)
         dlaToImage ps mi

