module Vector where
import System.Random

data Player = Player1 | Player2 | Tie deriving Show

data RPS = Rock | Paper | Scissors deriving (Show, Eq)
data Coin = Heads | Tails deriving (Show, Eq)

data Game = G RPS Coin

instance Ord RPS where
  (<=) Rock Paper = True
  (<=) Paper Rock = False
  (<=) Paper Scissors = True
  (<=) Scissors Paper = False
  (<=) Scissors Rock = True
  (<=) Rock Scissors = False

bout :: RPS -> RPS -> Player
bout p1 p2 | p1 == p2 = Tie
           | p1 > p2 = Player1
           | otherwise = Player2


getBinary :: (Random a, Num a) => Int -> [a]
getBinary seed = randomRs (0, 1) $ mkStdGen seed 

toss::  (Num a, RandomGen g, Random a) => g -> (a, g)
toss seed = randomR (0, 1) seed
-- toss seed | randomR (0, 1) seed == 0 = (Heads, (snd.randomR (0, 1)) seed)
          -- | otherwise = (Tails, (snd.randomR (0, 1)) seed)


-- srand :: a -> a
-- srand 