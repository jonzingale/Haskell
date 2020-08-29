module Helpers where
import Control.Monad.State
import Data.Bifunctor -- first
import System.Random
import DLA

-- State Monad: accumulate state within monad
-- random :: (RandomGen g, Random a) => g -> (a, g)
-- state random :: (RandomGen s, Random a, MonadState s m) => m a
threeCoins :: State StdGen (Bool,Bool,Bool)
threeCoins = do
  a <- randomSt
  b <- randomSt
  c <- randomSt
  return (a,b,c)
  where randomSt = state random

ex1 = runState threeCoins (mkStdGen 33)

blinkStates :: State StdGen Board
blinkStates = do
  let vals = iterate ((=<<) boardSt) $ return board
  val <- vals !! 10
  return val
  where
    rBoard :: RandomGen c => Board -> c -> (Board, c)
    rBoard = \b g -> first (flip blink b) $ random g
    boardSt bd = state $ rBoard bd

-- pass blinkStates an initial seed and get 10th board state
ex2 = runState blinkStates (mkStdGen 12)
