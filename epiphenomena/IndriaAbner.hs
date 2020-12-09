module IndriaAbner where
import System.Random

{--
Indria generates lists of various lengths
Abner sorts them with some error and bounded resources
--}

indria :: [[Int]]
indria =
  let rands = randoms (mkStdGen 42) in
  let splits = randomRs (0, 10^5) (mkStdGen 23) in
  f rands splits
  where
    f rs (s:ss) = take s rs : f (drop s rs) ss

data Body = B {
  buckets :: [[Int]],
  focus :: ([Int], [Int])
} deriving (Show)

class Abner a where
  classes :: a -> Int
  match :: a -> Bool

instance Abner Body where
  classes = length.buckets
  match x = let (a, b) = focus x in a == b


-- abner :: [Int] -> [Int] -> Bool
-- abner as bs =
  -- select random indices to verify
  -- with some small error compare them.