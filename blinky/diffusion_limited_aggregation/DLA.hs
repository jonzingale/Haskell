module DLA where
import Data.List (partition)
import System.Random

{-- Diffusion limited aggregation --}
data Board = B { frees :: [Free], bounds :: [Bound] } deriving (Show)
data Point = P Int Int deriving (Show, Eq)
type Bound = Point
type Free = Point
type Seed = Int 

genFrees :: Seed -> [Free]
genFrees s =
  let ns = randomRs (0, 9) $ mkStdGen (s+1)
      ms = randomRs (0, 9) $ mkStdGen s in
  [P a b | (a, b) <- zip ns ms]

randomStep :: Seed -> Point -> Point
randomStep s (P p q) =
  let (n, g) = randomR (-1, 1) $ mkStdGen s
      (m, _) = randomR (-1, 1) g in
  P (p + n) (q + m)

nearBound :: [Bound] -> Free -> Bool
nearBound bs fr = any (\b -> euclMet b fr <= 1) bs
  where
    euclMet (P b1 b2) (P f1 f2) =
      let f = \x -> fromIntegral x in
      sqrt $ (f b1 - f f1)^2 + (f b2 - f f2)^2 

blink :: Seed -> Board -> Board
blink seed (B fs bs) =
  let fs' = map (randomStep seed) fs in
  let (bs', fs'') = partition (\x -> nearBound bs x) fs' in
  B fs'' (bs' ++ bs)

-- TODO: incorporate state or writer monad, produce running example
-- work through the reasoning for prefilter etc...
example :: Seed -> IO ()
example seed = do
  let fs = take 40 $ genFrees seed
  let bd = blink seed $ B fs [P 5 5]
  print $ bounds bd

board :: Board
board = B (take 40 $ genFrees 42) [P 4 5]