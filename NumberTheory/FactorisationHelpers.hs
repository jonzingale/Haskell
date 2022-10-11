module FactorisationHelpers where
import Math.NumberTheory.Primes -- divisors
import Data.Set (singleton, Set, mapMonotonic, unions, toList)

listDivisors :: Integer -> [Integer]
listDivisors = tail.toList.divisors

divisors :: Integer -> Set Integer
divisors n
    | n < 0     = divisors (-n)
    | n == 0    = error "Can't create set of divisors of 0"
    | n == 1    = singleton 1
    | otherwise = divisorsFromCanonical (g.factorise $ n)
  where
    g ds = [(unPrime a, fromIntegral w) | (a, w) <- ds]


divisorsFromCanonical :: [(Integer, Int)] -> Set Integer
divisorsFromCanonical = foldl step (singleton 1)
  where
    step st (p,k) =
      unions (st:[mapMonotonic (*pp) st | pp <- take k (iterate (*p) p) ])