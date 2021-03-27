module LZW where
import Data.List (elemIndex)
import Data.Char (ord)

{-- Todo:
0. write any version
1. Write slick version
2. Write stateful monadic version
3. Write light weight version
--}

{--
  Notes:
  Here, chars prepend to registers, registers append to dictionaries.
  example from https://www2.cs.duke.edu/csed/curious/compression/lzw.html
--}

msg1 = "banana_bandana"
msg2 = "bandana_banana"

testMsg = lzwEncode msg1 == [1,0,3,6,0,4,5,3,2]

baseDict = map (\c -> [c]) ['a'..'z']
specDict = ["a","b","d","n","_"]

type Dictionary = [Register]
type Register = String
type Byte = Int

hasIndex :: Register -> Dictionary -> Bool
hasIndex sym dd = length [ n | (d, n) <- zip dd [0..], sym == d] > 0

encode :: Register -> Dictionary -> Byte
encode reg dict = head [ n | (d, n) <- zip dict [0..], reg == d]

lzwEncode :: String -> [Byte]
lzwEncode (s:str) = reverse $ f str specDict [s] [] -- NOTE which Dictionary
  where
    f [] _ _ code = code
    f [m] _ _ code = code -- what if the last char is unique to the message?
    f (m:msg) dict reg code
      -- extend register and try again
      | hasIndex (m:reg) dict = f msg dict (m:reg) code
      -- extend dictionary, swap register, extend code, and try again
      | otherwise = f msg (dict ++ [m:reg]) [m] (encode reg dict : code)

