module GeneratingFunctions where

instance Num a => Num [a] where
  fromInteger n = [fromInteger n]
  (x:xs) + (y:ys) = (x + y) : (xs + ys)
  xs + [] = xs
  [] + ys = ys
  (x:xs) * (y:ys) = (x*y) : ([x] * ys + xs * (y:ys))
  _ * _ = []

choose :: Int -> Int -> Int
choose n k = ([1,1]^n) !! k

fibs :: Int -> [Int] -- just not right somehow
fibs n = [1,-1,-1]^n

fibs' :: Double -> Integer
fibs' n = floor $ (1/sqrt 5) * (phi**n - pha**n)
  where
    phi = (1+sqrt 5) /2
    pha = (1-sqrt 5) /2