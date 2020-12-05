module DLAVector where
import qualified Data.Vector.Unboxed as U
import Data.Bifunctor (first, bimap)
import Control.Monad.State
import System.Random

{-- Diffusion limited aggregation --}
data Board = B {
  frees :: U.Vector Free,
  bounds :: U.Vector Bound
} deriving (Show)

type Bound = (Int, Int)
type Free = (Int, Int)
type Seed = Int

bsize = 400 :: Int -- board size
pcount = 12000 :: Int -- number of particles

board :: Board
board = B (genFrees 42) (U.singleton (div bsize 2, div bsize 2))
  where
    genFrees seed =
      let (g1, g2) = split.mkStdGen $ seed in
      let rs g = U.fromList.(take pcount).randomRs (0, bsize) $ g in
      U.zip (rs g1) (rs g2)

randomStep :: Seed -> Free -> Free
randomStep seed (p, q) =
  let (g1, g2) = split.mkStdGen $ seed in
  let (n, m) = (rr g1, rr g2) in
  ((p + n) `mod` bsize, (q + m) `mod` bsize)
  where rr = fst.randomR (-1, 1)

nearBound :: U.Vector Bound -> Free -> Bool
nearBound bs fr = U.any (\b -> dist b fr) bs
  where
    dist (b1, b2) (f1, f2) = (eball b1 f1) && (eball b2 f2)
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
