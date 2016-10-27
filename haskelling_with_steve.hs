module HaskellingWithSteve where

array = [1,2,3,4]
chars = ['a','v','c']

wax :: [String]
wax = ["fluke", "endgame"]


int2char x = show x

fun :: [Integer -> Integer]
fun = [(+2), (+3)]

len :: [a] -> Int
len [] = 0
len (s:teve) = 1 + (len teve)

fibs :: Int -> Int
fibs 0 = 0
fibs 1 = 1
fibs n = fibs (n-1) + fibs (n-2)

summ :: Int -> Int
summ n | div n 10 == 0 = n
       | otherwise = (mod n 10) + (summ (div n 10))