module DLA where
import Data.Bifunctor (first, bimap)
import Control.Monad.State
import System.Random

{-- Diffusion limited aggregation --}
data Board = B { frees :: [Free], bounds :: [Bound] } deriving (Show)
type Bound = (Int, Int)
type Free = (Int, Int)
type Seed = Int

size = 400

board :: Board
board = B (genFrees 42) [(5, 5)]
  where
    genFrees seed =
      let (g1, g2) = split.mkStdGen $ seed in
      let rs = randomRs (0, size) in
      take 4000 $ zip (rs g1) (rs g2)

randomStep :: Seed -> Free -> Free
randomStep seed (p, q) =
  let (g1, g2) = split.mkStdGen $ seed in
  let (n, m) = bimap rr rr (g1, g2) in
  ((p + n) `mod` size, (q + m) `mod` size)
  where rr = fst.randomR (-1, 1)

nearBound :: [Bound] -> Free -> Bool
nearBound bs fr = any (\b -> dist b fr) bs
  where
    dist (b1, b2) (f1, f2) = (eball b1 f1) && (eball b2 f2)
    eball a b = abs (a - b) <= 1

-- Absorb new bounds and then blink
blink :: Seed -> Board -> Board
blink seed (B fs bs) =
  let B fs' bs' = absorb fs bs [] in
  let rands = randoms (mkStdGen seed) :: [Int] in
  let blinkFrees = zipWith randomStep rands fs' in
  B blinkFrees bs'
    where
      absorb [] bs fs' = B fs' bs
      absorb (f:fs) bs fs'
        | nearBound bs f && elem f bs = absorb fs bs fs'
        | nearBound bs f = absorb fs (f:bs) fs'
        | otherwise = absorb fs bs (f:fs')

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
