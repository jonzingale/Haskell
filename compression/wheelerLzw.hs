module WheelerLZW where
import BurrowsWheeler
import Sort

msg1 = "banana_bandana"
msg2 = "TOBEORNOTTOBEORTOBEORNOT#"
msg3 = foldr (++) "" $ take 100 $ repeat msg1
dictionary = map (\x -> [x]) ('_':'#':'|':['a'..'z']++['A'..'Z'])

-- presorting compresses better
test1 = ((f.msort) msg3 < f msg3) == True
  where f = length.lzwEncode dictionary

-- burrows-wheelering before hand compresses better
test2 = ((f.bwt) msg3 < f msg3) == True
  where f = length.lzwEncode dictionary

-- burrows-wheelering before hand compresses worse
test3 = ((f.bwt) msg1 <= f msg1) == False
  where f = length.lzwEncode dictionary

type Register = String
type Dictionary = [Register]

hasIndex :: Register -> Dictionary -> Bool
hasIndex reg dict = f reg dict
  where
    f r (d:ds) = if d == r then True else f r ds
    f r [] = False

encode :: Register -> Dictionary -> Int
encode reg dict = f reg dict 0
  where
    f r (d:ds) n = if d == r then n else f r ds (n+1)

lzwEncode :: Dictionary -> String -> [Int]
lzwEncode d (s:str) = f str d [s] []
  where
    f [] _ _ code = code
    f (m:msg) dict reg code
      -- extend register and try again
      | hasIndex (m:reg) dict = f msg dict (m:reg) code
      -- extend dictionary, swap register, extend endcoded string, and try again
      | otherwise = f msg (dict ++ [m:reg]) [m] (encode reg dict : code)
