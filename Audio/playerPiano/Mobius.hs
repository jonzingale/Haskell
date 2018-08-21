module Mobius where
import Data.Set (singleton, Set, mapMonotonic, unions, toList)
import Math.NumberTheory.Primes.Factorisation
import qualified Data.Vector.Unboxed as U
import Data.Numbers.Primes

listDivisors :: Integer -> [Integer]
listDivisors = tail.toList.divisors

divisors :: Integer -> Set Integer
divisors n
    | n < 0     = divisors (-n)
    | n == 0    = error "Can't create set of divisors of 0"
    | n == 1    = singleton 1
    | otherwise = divisorsFromCanonical (factorise' n)

divisorsFromCanonical :: [(Integer,Int)] -> Set Integer
divisorsFromCanonical = foldl step (singleton 1)
  where
    step st (p,k) = unions (st:[mapMonotonic (*pp) st | pp <- take k (iterate (*p) p) ])

mobius :: Integer -> Integer
mobius n = f.factorise' $ n
  where
    f ds | ds == [(1,1)] = 1
         | any (> 1) $ map snd ds = 0
         | even.length $ ds = 1
         | otherwise = -1

mobiusV :: [[Double]]
mobiusV = [map (fromIntegral.mobius) $ listDivisors k | k<-[1..]]

paddedMobius = map ((U.fromList).(take 12).(flip (++) $ repeat 0)) mobiusV
