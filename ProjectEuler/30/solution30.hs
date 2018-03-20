module Solution where

{--
Surprisingly there are only three numbers that can be written as the sum
of fourth powers of their digits:

1634 = 14 + 64 + 34 + 44
8208 = 84 + 24 + 04 + 84
9474 = 94 + 44 + 74 + 44
As 1 = 14 is not a sum it is not included.

The sum of these numbers is 1634 + 8208 + 9474 = 19316.

Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.
--}

-- 443839

euler30 = f (10^6)
  where
    f 0 = 0
    f n | (sum.fifths.toList) n == n = n + f (n-1)
        | otherwise = f (n-1)

fifths :: [Int] -> [Int]
fifths = \ary-> map (^5) ary

toList :: Int -> [Int]
toList 0 = []
toList n = toList (div n 10) ++ [mod n 10]