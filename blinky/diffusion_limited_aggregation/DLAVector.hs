module DLAVector where
import qualified Data.Vector.Unboxed as U
import Data.Bifunctor (first, bimap)
import Constants (bsize, pcount, hsize)
import Control.Monad.State
import System.Random

-- TODO: Consider GPU
-- http://hackage.haskell.org/package/accelerate-1.2.0.1/docs/Data-Array-Accelerate.html

{-- Diffusion limited aggregation --}
data Board = B {
  frees :: U.Vector Free,
  bounds :: U.Vector Bound
} deriving (Show)

type Bound = (Int, Int)
type Free = (Int, Int)
type Seed = Int

board :: Board
board = B (genFrees 42) (U.singleton (hsize, hsize))
  where
    genFrees seed =
      let (g1, g2) = split.mkStdGen $ seed in
      let rs = randomRs (0, bsize) in
      U.fromList $ take pcount $ zip (rs g1) (rs g2)

randomStep :: Seed -> Free -> Free
randomStep seed (p, q) =
  let (g1, g2) = split.mkStdGen $ seed in
  let rr = fst.randomR (-1, 1) in
  (mod (p + rr g1) bsize, mod (q + rr g2) bsize)

nearBound :: U.Vector Bound -> Free -> Bool
nearBound bs fr = U.any (dist fr) bs
  where
    dist (f1, f2) (b1, b2) = (eball b1 f1) && (eball b2 f2)
    eball a b = abs (a - b) <= 1

-- Absorb new bounds and then blink
blink :: Seed -> Board -> Board
blink seed (B fs bs) =
  let B fs' bs' = absorb fs bs in
  let len = U.length fs' in
  let rands = U.fromList.(take len).randoms $ mkStdGen seed in
  let fss = U.zipWith randomStep rands fs' in
  B fss bs'
    where
      -- Become bound if near and unique value
      boundCond bs f = nearBound bs f && (not $ U.elem f bs)
      absorb fs bs =
        let (bs', fs') = U.partition (boundCond bs) fs in
        B fs' ((U.++) bs' bs)

-- State Monad: accumulate state within monad
blinkStates :: Int -> Board -> State StdGen Board
blinkStates n b = iterateN n (return b)
  where
    iterateN 0 v = v
    iterateN k v = iterateN (k-1) (boardSt =<< v)
    -- random :: (RandomGen g, Random a) => g -> (a, g)
    rBoard :: RandomGen c => Board -> c -> (Board, c)
    rBoard = \b g -> first (flip blink b) $ random g
    boardSt = state.rBoard

-- Pass (blinkStates n board) an initial seed and get nth board state.
ex1 n = runState (blinkStates n board) (mkStdGen 12)
