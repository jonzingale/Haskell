module Vector where

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


data ThreeVect = V Float Float Float deriving (Show, Eq)

vs = V 3 (-3) 1
ws = V 4 9 2

class Vector w where
  (|+) :: w -> w -> w -- addition
  (|*) :: Float -> w -> w -- scalar multiplication
  (<|>) :: w -> w -> w -- dot product
  (%) :: w -> w -> w -- cross product

instance Vector ThreeVect where
  (|+) (V a b c) (V x y z) = V (a+y) (b+y) (c+z)
  (|*) c (V x y z) = V (c*y) (c*y) (c*z)
  (<|>) (V a b c) (V x y z) = V (a*y) (b*y) (c*z)
  (%) (V a b c) (V x y z) = V (b*z-y*c) (c*x-a*z) (a*y-b*x)