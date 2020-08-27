module Helpers where
import Control.Monad.State
import System.Random
import Data.Bifunctor
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

blinkState :: State Board Board
blinkState = do
  tal <- boardSt
  ual <- boardSt
  val <- boardSt
  return val
  where boardSt = state $ \b -> (b, blink 12 b)

ex2 = runState blinkState board

{--
It may be best to use iterate and allow the state
to accumulate in random gens. Maybe bifunctor so that
randStep :: g -> (Board, g), which becomes new State.
--}

blinkStates :: State StdGen Board
blinkStates = do
  tal <- randomSt
  ual <- randomSt
  val <- randomSt
  let b1 = blink tal board
  let b2 = blink ual b1
  let b3 = blink val b2
  return b3
  where
    randomSt = state random
    -- rBoard :: RandomGen g => g -> (Board, g)
    rBoard g = (blink (fst.random $ g) board, g)
    bRoard g = \b -> first (flip blink b) $ random g

ex3 n = fst $ runState blinkStates (mkStdGen n)

blinkStates' :: State StdGen Board
blinkStates' = do
  val <- boardSt board
  return val
  where
    boardSt bd = state $ rBoard bd
    rBoard = \b g -> first (flip blink b) $ random g -- consumes b first

-- ex4 = fst $ runState blinkStates' (mkStdGen 12)
