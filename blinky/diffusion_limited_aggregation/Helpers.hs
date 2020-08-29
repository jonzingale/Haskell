module Helpers where
import Control.Monad.State
import Data.Bifunctor -- first
import System.Random
import DLA

-- State Monad: accumulate state within monad
-- random :: (RandomGen g, Random a) => g -> (a, g)

blinkStates :: Int -> State StdGen Board
blinkStates n = do
  let vals = iterate ((=<<) boardSt) $ return board
  val <- vals !! n
  return val
  where
    rBoard :: RandomGen c => Board -> c -> (Board, c)
    rBoard = \b g -> first (flip blink b) $ random g
    boardSt bd = state $ rBoard bd

-- pass (blinkStates n) an initial seed and get nth board state
ex1 n = runState (blinkStates n) (mkStdGen 12)
