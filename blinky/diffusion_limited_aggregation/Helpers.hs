module Helpers where
import Control.Monad.Writer
import System.Random
import DLA

listboard :: [[Int]]
listboard = f.(take 100) $ map g randos
  where
    f [] = []
    f ls = take 10 ls : (f.drop 10 $ ls) 
    randos = randoms $ mkStdGen 99 :: [Int]
    g x = case (x `mod` 20) of
      0 -> 1
      _ -> 0

-- Writer Monad

output :: Int -> Board -> Writer [Free] Board
output n bd
  | n == 0 = do
    tell $ bounds bd
    return bd
  | otherwise = do
    tell $ bounds bd
    output (n-1) (blink 12 bd)