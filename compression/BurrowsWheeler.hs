module BurrowsWheeler (bwt, inv_bwt, julia) where
import Data.List (sort)

-- given a string without a '|', return the burrows-wheeler transform
-- NOTE: sort is true lexigraphical order, not just the first character!

julia = "julia"
banana = "^BANANA"

--------------forward method below
bwt :: String -> String
bwt xs = last.trans.sort.(take $ length xs+1).rotate $ '|' : xs

rotate :: [a] ->[[a]]
rotate (x:xs) = (x:xs) : rotate (xs ++ [x])

--------------inverse method below
inv_bwt :: String -> String
inv_bwt [] = []
inv_bwt zs = head [ b | b <- block zs [], (head.reverse) b == '|' ]

block :: Ord a => [a] -> [[a]] -> [[a]]
block xs [] = block xs [xs]
block xs xss
  | length xs == length xss = trans xss
  | otherwise = block xs $ xs : (trans.sort.trans $ xss)

--- this exists in Data.List but was fun to write anyway
trans :: [[a]] -> [[a]]
trans [] = []
trans ([]:xss) = trans xss
trans xs = (map head xs) : trans (map tail xs)
