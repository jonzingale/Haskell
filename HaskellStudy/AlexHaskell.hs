module AlexHaskell where

fibs :: [Integer]
fibs = [0,1] ++ zipWith (+) fibs (tail fibs)

lengthRecursion :: [a] -> Integer
lengthRecursion [] = 0
lengthRecursion [x] = 1
lengthRecursion (x:xs) = lengthRecursion [x] + lengthRecursion xs

ns :: Integer -> Integer -> [Integer]
ns l u | l > u = []  
       | otherwise = l : ns (l+1) u

ms :: Integer -> Integer -> [Integer]
ms l 0 = []
ms l u = l : ms (l+1) (u-1)

-- take, drop, length, (!!), head, tail, (++)
type BinOp = Integer -> Integer -> Integer

(!!!), (+++), dropN, takeN :: BinOp
(!!!) zs n = headN.dropN n $ zs
dropN n zs = div zs (10^n)
takeN n zs = mod zs (10^n)
headN = takeN 1
tailN = dropN 1

(+++) 0 ns = ns
(+++) ns 0 = ns * 10
(+++) ns ms = ms + ns * 10^lengthN ms

lengthN :: Integer -> Integer
lengthN 0 = 0
lengthN n = 1 + lengthN (div n 10)

-- comb 0 1 = 0
-- comb 1 0 = 1
-- comb 1 1 = 1
-- comb n k = comb (n-1) (k-1) + comb (n-1) k


