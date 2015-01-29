module BurrowsWheeler (bwt,inv_bwt,julia) where
import Data.Char
import Data.List 

-- given a string without a |, return the burrows-wheeler transform

julia = "julia"
banana = "^BANANA"

--------------forward method below
bwt xs = (last.trans.sort.(take (length xs+1)).rotate) ("|"++xs)

rotate :: [a] ->[[a]]
rotate (x:xs) = (x:xs) : rotate (xs++[x])

--------------inverse method below
inv_bwt str = map chr $ inv_bwt_int $ map ord str

inv_bwt_int :: [Int] -> [Int]
inv_bwt_int [] = []
inv_bwt_int zs = head [b|b<-block zs [],(head.reverse)b==124 ]

block :: Ord a =>[a] -> [[a]] ->[[a]]
block xs [] = block xs [xs]
block xs xss | length xs == length xss = trans xss
			 | otherwise = block xs $ xs: ((trans.sort.trans) xss)

--- this exists in Data.List but was fun to write anyway
trans :: [[a]] -> [[a]]
trans [] = []
trans ([]:xss) = trans xss -- the curious case?
trans xs = (map head xs) : trans (map tail xs)

