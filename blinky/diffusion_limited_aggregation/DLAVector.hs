module DLAVector where
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as G
import Codec.Picture -- JuicyPixel
import Conversion -- conversion-1.2.1
import System.Random

{-- Diffusion limited aggregation --}
data Board =
  B { frees :: U.Vector Free, bounds :: U.Vector Bound } deriving (Show)

type Bound = (Int, Int)
type Free = (Int, Int)
type Seed = Int 

board :: Board
board = B (U.take 40 $ genFrees 42) (U.singleton (1, 2))

-- todo: write monadically and use U.generate
genFrees :: Seed -> U.Vector Free
genFrees s =
  let ns = randomRs (0, 9) $ mkStdGen (s+1)
      ms = randomRs (0, 9) $ mkStdGen s in
  U.fromList $ zip ns ms