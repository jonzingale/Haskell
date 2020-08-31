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
board = B (U.take 40 $ genFrees 42) (U.singleton (4, 4))

-- todo: write monadically over random and use U.generate
genFrees :: Seed -> U.Vector Free
genFrees seed =
  let (g1, g2) = split $ mkStdGen seed in
  let ns = U.fromList $ randomRs (0, 9) g1
      ms = U.fromList $ randomRs (0, 9) g2 in
  U.zip ns ms

