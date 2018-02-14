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

{--
Create a Three Vector data type
extend type to num class:
  - vector addition
  - dot product
  - difference
  - fromInteger (for scalar multiplication)
  - norm

introduce functor class
  - rewrite ThreeVect as V a a a
  - instance Floating v => Num (ThreeVect v) where
  - fmap <$> makes abs, signum and fromInteger easy!
--}
data ThreeVect = V Float Float Float | ComplexRoot deriving (Show, Eq)

vs = V 3 (-3) 1
ws = V 4 9 2

class Vector w where
  (%) :: w -> w -> w -- cross product

instance Vector ThreeVect where
  (%) (V a b c) (V x y z) = V (b*z-y*c) (c*x-a*z) (a*y-b*x)

instance Num ThreeVect where
  (+) (V a b c) (V x y z) = V (a+x) (b+y) (c+z)
  (*) (V a b c) (V x y z) = V (a*y) (b*y) (c*z) -- dot product
  (-) (V a b c) (V x y z) = V (a-x) (b-y) (c-z)
  signum (V a b c) = V (signum a) (signum b) (signum c)
  abs vect = squareRoot $ vect * vect  

  -- fascilitates scalar multiplication
  fromInteger x = V (fromInteger x) (fromInteger x) (fromInteger x)

squareRoot :: ThreeVect -> ThreeVect
squareRoot (V a b c) = V (sqrt a) (sqrt b) (sqrt c)


