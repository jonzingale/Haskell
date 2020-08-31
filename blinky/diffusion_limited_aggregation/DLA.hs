module DLA where
import Data.Bifunctor (first, bimap)
import Control.Monad.State
import System.Random

{-- Diffusion limited aggregation --}
data Board = B { frees :: [Free], bounds :: [Bound] } deriving (Show)
type Bound = (Int, Int)
type Free = (Int, Int)
type Seed = Int 

board :: Board
board = B (take 40 $ genFrees 42) [(5, 5)]

genFrees :: Seed -> [Free]
genFrees seed =
  let (g1, g2) = split.mkStdGen $ seed
      rs = randomRs (0, 9) in
  zip (rs g1) (rs g2)

randomStep :: Seed -> Free -> Free
randomStep seed (p, q) =
  let (g1, g2) = split $ mkStdGen seed in
  let (n, _) = randomR (-1, 1) g1
      (m, _) = randomR (-1, 1) g2 in
  (p + n, q + m)

nearBound :: [Bound] -> Free -> Bool
nearBound bs fr = any (\b -> dist b fr) bs
  where
    dist (b1, b2) (f1, f2) = (eball b1 f1) && (eball b2 f2)
    eball a b = abs (a - b) <= 1

-- TODO: map (randomStep seed) causes drift
blink :: Seed -> Board -> Board
blink seed (B fs bs) =
  -- absorb new bounds and then blink
  let B fs' bs' = absorb fs bs [] in
  B (map (randomStep seed) fs') bs'
    where
      absorb [] bs fs' = B fs' bs
      absorb (f:fs) bs fs'
        | nearBound bs f && elem f bs = absorb fs bs fs'
        | nearBound bs f = absorb fs (f:bs) fs'
        | otherwise = absorb fs bs (f:fs')

-- State Monad: accumulate state within monad
-- random :: (RandomGen g, Random a) => g -> (a, g)
blinkStates :: Int -> Board -> State StdGen Board
blinkStates n b = do
  let vals = iterate ((=<<) boardSt) $ return b
  val <- vals !! n
  return val
  where
    rBoard :: RandomGen c => Board -> c -> (Board, c)
    rBoard = \b g -> first (flip blink b) $ random g
    boardSt bd = state $ rBoard bd

-- Pass (blinkStates n board) an initial seed and get nth board state.
ex1 n = runState (blinkStates n board) (mkStdGen 12)
