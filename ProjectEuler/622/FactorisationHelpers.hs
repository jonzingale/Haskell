module FactorisationHelpers where
import Math.NumberTheory.Primes.Factorisation -- divisors
import Data.Set (singleton, Set, mapMonotonic, unions, toList)

-- just give proper divisors
properDivisors :: Integer -> [Integer]
properDivisors n = f.tail.toList.divisors $ n
  where
    f [x] = []
    f (x:xs) = x:f xs

listDivisors :: Integer -> [Integer]
listDivisors = tail.toList.divisors

-- modified to just give proper divisors
divisors :: Integer -> Set Integer
divisors n
    | n < 0     = divisors (-n)
    | n == 0    = error "Can't create set of divisors of 0"
    | n == 1    = singleton 1
    | otherwise = divisorsFromCanonical (factorise' n)

divisorsFromCanonical :: [(Integer,Int)] -> Set Integer
divisorsFromCanonical = foldl step (singleton 1)
  where
    step st (p,k) = unions (st:[mapMonotonic (*pp) st | 
                            pp <- take k (iterate (*p) p) ])