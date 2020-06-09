module SevensTrick where

number = 300^300-1

sevensTrick :: Integer -> Bool
sevensTrick n | n > 7 = sevensTrick $ (div n 10) - 2 * (mod n 10)
              | otherwise = any (== n) [-7, 0, 7]

-- https://twitter.com/UKMathsTrust/status/1269913635087736832
problem50 = sevensTrick number