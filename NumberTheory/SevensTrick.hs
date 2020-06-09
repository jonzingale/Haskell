module SevensTrick where

number = 300^300-1

sevensTrick :: Integer -> Bool
sevensTrick n | n == 0 || n == 7 || n == -7 = True
              | n > 7 = sevensTrick ((div n 10) - 2 * (mod n 10))
              | otherwise = False
