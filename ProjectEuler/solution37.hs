module Solution where
import Data.Numbers.Primes -- isPrime, primes
import Prelude hiding (head, tail, length, last)

class (Ord s, Show s) => Sortable s where
  head, tail :: s -> s
  length :: s -> Int
  unit :: s

instance Sortable Integer where
  head n = mod n 10
  tail n = div n 10
  unit = 0

  length 0 = 0
  length n = 1 + (length.div n) 10

heads :: Integer -> Integer
heads n = n `mod` 10^(length n - 1)

stillPrime :: Integer -> Bool
stillPrime n | length n == 1 && isPrime n = True
             | isPrime n = lefts n && rights n
             | otherwise = False
  where
    lefts n | length n == 1 && isPrime n = True
            | otherwise = isPrime n && (lefts.tail) n

    rights n | length n == 1 && isPrime n = True
             | otherwise = isPrime n && (rights.heads) n

euler37 = f (drop 4 primes) 0 11
  where
    f (p:ps) a 0 = a
    f (p:ps) a i | stillPrime p = f ps (a+p) (i-1)
                 | otherwise = f ps a i