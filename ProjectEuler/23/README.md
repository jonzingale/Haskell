
### Non-abundant sums

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


```haskell
import Data.Set (Set, (\\), singleton, empty, unions)

listToSet :: Ord a => [a] -> Set a
listToSet list = unions $ map singleton list
```

The strategy for this problem is to utilize laziness and the `Data.Set` library.<br>
The initial hint is to notice that for all n > 28123, ∃ j, k ∈ AbundantNumbers such that j+k = n.<p>
The plan is as follows:
- Find all n < 28124 which are the sum to two abundant numbers.
- Remove these numbers from the list of numbers `1..28123`.
- Take the sum of this difference.


```haskell
sigmaDivisors :: Integer -> Integer
sigmaDivisors n = 1 + sum [sig i n | i <- [2..root n], mod n i == 0]
  where
    root = floor.sqrt.fromInteger
    sig i n | i^2 == n = i
            | otherwise = i + div n i  

abundant :: Integer -> Bool
abundant n = n < sigmaDivisors n
```

`sigmaDivisors` finds the sum of the proper divisors for a given n.<br>
That we need only calculate up to the square root of n speeds up the<br>
calculation at the expense of needing to be careful at the square root.<br>
Now, `abundant :: Integer -> Bool` can be written to distinguish those<br>
which are from those that are not abundant.


```haskell
sumAbundantPairs :: Integer -> [Integer] -- sumAbundantPairs 10 => [4+4]
sumAbundantPairs n = [x+y | x <- abundants, y <- abundants]
  where abundants = filter abundant [1..n]
  
challenge23 =  sum.diff [1..28123] $ sumAbundantPairs 28123
  where
    diff = \ x y -> listToSet x \\ listToSet y
```

`sumAbundantPairs` now takes the cartesian product of the abundant numbers<br>
up to n with itself and returns the sum of the pairs. `challenge23` now <br>
computes the project euler solution by taking the Set-theoretic difference of<br>
these sums from the set of all numbers up to 28123. Those that remain are those<br>
which could not be written as the sum of two abundant numbers.
