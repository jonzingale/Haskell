module LZW where

{--
  Notes:
  Here, chars prepend to registers, registers append to dictionaries.
  example from https://www2.cs.duke.edu/csed/curious/compression/lzw.html

  If last character is unique to message, how is it recovered in decoding?
--}

msg1 = "banana_bandana"
msg2 = "bandana_banana"

test = lzwEncode msg1 == reverse [1,0,3,6,0,4,5,3,2]

baseDict = map (\c -> [c]) ['a'..'z']
specialDict = ["a","b","d","n","_"]

type Dictionary = [Register]
type Register = String

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
lzwEncode (s:str) = f str specialDict [s] [] -- NOTE specified Dictionary
  where
    f [] _ _ code = code
    f [m] _ _ code = code
    f (m:msg) dict reg code
      -- extend register and try again
      | hasIndex (m:reg) dict = f msg dict (m:reg) code
      -- extend dictionary, swap register, extend code, and try again
      | otherwise = f msg (dict ++ [m:reg]) [m] (encode reg dict : code)
