module EfficientBWT (bwt, inv_bwt, julia, julias) where
import Sort (msort, rsort)

{--
Notes:
- Given a string without a '|', return the burrows-wheeler transform
- msort is true lexigraphical order, not just the first character!
- msort is the same efficient implementation from Data.List
- rsort is a modified qsort with randomized input optimized for the BWT, nearly
  as fast as msort, much better on space.
--}

julia = "julia"
banana = "BANANA"
julias = take 1000 $ foldr (++) "" $ repeat "julia"

--------------forward method below
bwt :: String -> String
bwt xs = last.trans.msort.(take $ length xs+1).rotate $ '|' : xs

eff_bwt :: String -> String
eff_bwt xs = rsort.(take $ length xs+1).rotate $ '|' : xs

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
  | otherwise = block xs $ xs : (trans.msort.trans $ xss)

--- this exists in Data.List but was fun to write anyway
trans :: [[a]] -> [[a]]
trans [] = []
trans ([]:xss) = trans xss
trans xs = (map head xs) : trans (map tail xs)
