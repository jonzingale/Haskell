module BestHorse where
import Math.NumberTheory.Primes.Factorisation
import Data.Numbers.Primes
import Text.Printf
import Data.Set

bestHorse :: Integer
bestHorse = f 10000000000 2602680992096 -- 10^9
  where
    f 1000000000000 acum = acum -- 10^11
    f n acum | gg' n && ff' n = f (n+2) (n+acum)
             | otherwise = f (n+2) acum

    ff' n = and [mod i (n-1) /= 1 | i <- [1073741824, 1048576, 32768, 4096]] -- [30,20,15,12]
    gg' n = mod 1152921504606846976 (n-1) == 1 -- 2^60

testable :: Integer
testable = f 4 0 -- == 135411214 -- 10^8
  where
    f 1000000 acum = acum -- 10^9
    f n acum | gg' n && ff' n = f (n+2) (n+acum)
             | otherwise = f (n+2) acum

    ff' n = and [mod i (n-1) /= 1 | i <- [1073741824, 1048576, 32768, 4096]] -- [30,20,15,12]
    gg' n = (== empty).difference (fromList.primeFactors $ n-1) $ factors
    factors = fromList [3,5,7,11,13,31,41,61,151,331,1321]
    -- gg' n = mod 1152921504606846975 (n-1) == 0 -- 2^60-1



{--
Factors for 2^60-1:
  [[3,2],[5,2],[7,1],[11,1],[13,1],[31,1],[41,1],[61,1],[151,1],[331,1],[1321,1]]

either:
factorise 1152921504606846975
primeFactors 1152921504606846975
--}




main = do
  let format = "Test Match 135411214 == %i\nReal Deal: %i\n"
  putStr $ printf format testable bestHorse
