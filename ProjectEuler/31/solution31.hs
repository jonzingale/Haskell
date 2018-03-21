{-# OPTIONS_GHC -Wno-missing-methods #-}

module Solution where

{--
In England the currency is made up of pound, £, and pence, p, 
and there are eight coins in general circulation:

1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
It is possible to make £2 in the following way:

1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
How many different ways can £2 be made using any number of coins?
--}

instance Num a => Num [a] where
  fromInteger n = [fromInteger n]
  (x:xs) + (y:ys) = (x + y) : (xs + ys)
  xs + [] = xs
  [] + ys = ys
  (x:xs) * (y:ys) = (x*y) : ([x] * ys + xs * (y:ys))
  _ * _ = []

choose :: Int -> Int -> Int
choose n k = ([1,1]^n) !! k

us, uk :: [Int]
us = [1,5,10,25,50,100]
uk = [1,2,5,10,20,50,100,200]

zeros :: Int -> [Int]
zeros n = (++ [1]).take (n-1) $ repeat 0

cycles bound n =  1 : (take bound $ cycle.zeros $ n)

euler31 = (!!200).foldr (*) 1 $ map (cycles 200) uk
us31 = (!!100).foldr (*) 1 $ map (cycles 100) us
us31Correct = (!!101).foldr (*) 1 $ map (cycles 100) us

{-- 
I suspect the problem is off by one, 201st not 200th. For a dollar of
US currency, the answer ought to be 292, but the 100th value is 293.
The 101th value is 292, which ought to be the correct answer. so..hmm.
--}