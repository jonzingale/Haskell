module Nemo where


nemo :: String
nemo = "Nemo"

f :: String -> Int
f str = 1

-- takes a string and a list of strings
-- and returns true or false if the string
-- is in the list.
friends :: String -> Bool
friends name
  | name == "jon" = True
  | name == nemo = True
  | name == "david" = True
  | otherwise = False

yes :: Bool -> String
yes b
  | b == True = "Yes"
  | otherwise = "Hell No"

project :: [(Int, String)] 
project = [(1, "ProG"), (2, "ProJ")]

percent :: [(Int, String)] -> Int
percent prj
  | (head prj) == (1, "ProG") = 2
  | otherwise = 0

len :: [a] -> Int
len [] = 0
len list = 1 + len (tail list)

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)