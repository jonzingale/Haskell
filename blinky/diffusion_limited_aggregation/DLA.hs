module DLA where
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
nearBound bs fr = any (\b -> euclMet b fr <= sqrt 2) bs
  where
    euclMet (P b1 b2) (P f1 f2) = let f = fromIntegral in
      sqrt $ (f b1 - f f1)^2 + (f b2 - f f2)^2 

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

-- TODO: incorporate state or writer monad, produce running example
-- work through the reasoning for prefilter etc...
board :: Board
board = B (take 40 $ genFrees 42) [P 4 5]

example :: Seed -> IO ()
example seed = do
  let bd = blink seed board
  print $ bounds bd
