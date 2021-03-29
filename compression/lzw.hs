module LZW where

{--
  Notes:
  - Example from https://www2.cs.duke.edu/csed/curious/compression/lzw.html
  - Here, chars prepend to registers, registers append to dictionaries.
  - If last character is unique to message, how is it recovered in decoding?
--}

msg1 = "banana_bandana"
dictionary = ["a","b","d","n","_"]

test = lzwEncode msg1 == [2,3,5,4,0,6,3,0,1]

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

lzwEncode :: String -> [Int]
lzwEncode (s:str) = f str dictionary [s] [] -- NOTE: example specific dictionary
  where
    f [] _ _ code = code
    f (m:msg) dict reg code
      -- extend register and try again
      | hasIndex (m:reg) dict = f msg dict (m:reg) code
      -- extend dictionary, swap register, extend endcoded string, and try again
      | otherwise = f msg (dict ++ [m:reg]) [m] (encode reg dict : code)
