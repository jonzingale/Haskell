module DLA where
import System.Random

{-- Diffusion limited aggregation --}
data Point = P Int Int deriving (Show, Eq)
type Bound = Point
type Free = Point
type Seed = Int 

-- rewrite in record syntax
data Board = B [Free] [Bound] deriving (Show)
pr1 (B f b) = f
pr2 (B f b) = b

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
      sqrt $ (f b1-f f1)^2 + abs (f b2-f f2)^2 

blink :: Seed -> Board -> Board
blink seed (B fs bs) =
  let fs' = map (randomStep seed) fs in
  let (bs', fs'') = partition (\x -> nearBound bs x) fs' in
  B fs'' (bs' ++ bs)

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition f as = rr f as ([],[])
  where
    rr f [] accum = accum
    rr f (l:ls) (as, bs)| f l = rr f ls (l:as, bs)
                        | otherwise = rr f ls (as, l:bs)

-- TODO: incorporate state or writer monad, produce running example
-- work through the reasoning for prefilter etc...
example :: Seed -> IO ()
example seed = do
  let fs = take 40 $ genFrees seed
  let bs = [P 5 5]
  -- reckons by getting initial near bounds before blink
  let (bs', fs') = partition (\x -> nearBound bs x) fs
  -- perform blink on reckoned board
  let bd = blink seed $ B fs' (bs ++ bs')
  print $ pr2 bd

board :: [[Int]]
board = f.(take 100) $ map g randos
  where
    f [] = []
    f ls = take 10 ls : (f.drop 10 $ ls) 
    randos = randoms $ mkStdGen 99 :: [Int]
    g x = case (x `mod` 20) of
      0 -> 1
      _ -> 0


