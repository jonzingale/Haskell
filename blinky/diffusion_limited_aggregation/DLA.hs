module DLA where
import Data.Bifunctor (first, bimap)
import Constants (bsize, pcount)
import Data.List (partition)
import Control.Monad.State
import System.Random

{-- Diffusion limited aggregation --}
data Board = B { frees :: [Free], bounds :: [Bound] } deriving (Show)
type Bound = (Int, Int)
type Free = (Int, Int)
type Seed = Int

board :: Board
board = B (genFrees 42) [(5, 5)]
  where
    genFrees seed =
      let (g1, g2) = split.mkStdGen $ seed in
      let rs = randomRs (0, bsize) in
      take pcount $ zip (rs g1) (rs g2)

randomStep :: Seed -> Free -> Free
randomStep seed (p, q) =
  let (g1, g2) = split.mkStdGen $ seed in
  let (n, m) = bimap rr rr (g1, g2) in
  ((p + n) `mod` bsize, (q + m) `mod` bsize)
  where rr = fst.randomR (-1, 1)

nearBound :: [Bound] -> Free -> Bool
nearBound bs fr = any (dist fr) bs
  where
    dist (f1, f2) (b1, b2) = (eball b1 f1) && (eball b2 f2)
    eball a b = abs (a - b) <= 1

-- Absorb new bounds and then blink
blink :: Board -> Seed -> Board
blink (B fs bs) seed =
  let (fs', bs') = absorb fs bs in
  let rands = randoms (mkStdGen seed) in
  let fss = zipWith randomStep rands fs' in
  B fss bs'
  where
    -- Become bound if near and unique value
    boundCond bs f = nearBound bs f && (not.elem f $ bs)
    absorb fs bs =
      let (bs', fs') = partition (boundCond bs) fs in
      (fs', bs' ++ bs)

-- State Monad: accumulate state within monad
blinkStates :: Int -> Board -> State StdGen Board
blinkStates n b = iterateN n (return b)
  where
    iterateN 0 v = v
    iterateN k v = iterateN (k-1) (boardSt =<< v)
    -- random :: (RandomGen g, Random a) => g -> (a, g)
    rBoard :: RandomGen c => Board -> c -> (Board, c)
    rBoard = \b g -> first (blink b) $ random g
    boardSt = state.rBoard

-- Pass (blinkStates n board) an initial seed and get nth board state.
ex1 n = runState (blinkStates n board) (mkStdGen 12)
