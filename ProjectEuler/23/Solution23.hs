module Solution23 where
{--
Non-abundant sums
Problem 23 
A perfect number is a number for which the sum of its proper divisors is
exactly equal to the number. For example, the sum of the proper divisors
of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

A number n is called deficient if the sum of its proper divisors is less than n
and it is called abundant if this sum exceeds n.

As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16,
the smallest number that can be written as the sum of two abundant
numbers is 24. By mathematical analysis, it can be shown that all integers
greater than 28123 can be written as the sum of two abundant numbers.

However, this upper limit cannot be reduced any further by analysis even
though it is known that the greatest number that cannot be expressed as the
sum of two abundant numbers is less than this limit.

Find the sum of all the positive integers which cannot be written
as the sum of two abundant numbers.
--}

-- :set +s for testing run time speed

import Data.Set (Set, (\\), singleton, empty, unions)

sigmaDivisors :: Integer -> Integer
sigmaDivisors n = 1 + sum [sig i n | i <- [2..root n], mod n i == 0]
  where
    root = floor.sqrt.fromInteger
    sig i n | i^2 == n = i
            | otherwise = i + div n i  

abundant :: Integer -> Bool
abundant n = n < sigmaDivisors n

sumAbundantPairs :: Integer -> [Integer] -- sumAbundantPairs 10 => [4+4]
sumAbundantPairs n = [x+y | x <- abundants, y <- abundants]
  where abundants = filter abundant [1..n]

listToSet :: Ord a => [a] -> Set a
listToSet list = unions $ map singleton list

challenge23 =  sum.diff [1..28123] $ sumAbundantPairs 28123
  where
    diff = \ x y -> listToSet x \\ listToSet y
