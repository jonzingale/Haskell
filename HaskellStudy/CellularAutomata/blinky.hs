module Blinky where
import System.Random

type Seed = Int
data Board = B [Bool]

instance Show Board where
  show (B []) = ""
  show (B (b:bs)) | b = "*" ++ show (B bs)
                  | otherwise = " " ++ show (B bs)

randomState :: Int -> Seed -> [Bool]
randomState len = (take len).randomRs(False,True).mkStdGen

board n = B $ randomState 80 n

rule n ls | n == 0 = True
          | n == length ls - 1 = True
          | otherwise = blink n ls
  where
    blink n ls = case neighborhood n ls of
      [False,False,False] -> False
      [False,True,False] -> False
      [True,True,True] -> False
      [True,False,True] -> False

      [False,True,True] -> True
      [True,False,False] -> True
      [True,True,False] -> True
      [False,False,True] -> True

neighborhood n ls = ls!!(n-1) : ls!!n : ls!!(n+1) : []

blinkOnce (B bs) = B [rule i bs | i<-[0..length bs - 1]]

main = do
  let boards = iterate blinkOnce $ board 22
  (putStr.unlines.map show) $ boards