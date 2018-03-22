module BestHorse where
import Data.Numbers.Primes -- primeFactors
import Text.Printf
import Data.Set

two60Minus1 = 1152921504606846975
smallsMinus1 = [1073741823, 1048575, 32767, 4095] -- [2^30-1, etc...]
smallLimit = 1073741823

{--
Scales linearly by tens, I need 10^18 which is ~ 3000 years
10^2 => 62
10^3 => 11528
10^4 => 400002
10^5 => 8232188
10^6 => 135411214 -- 0.48 secs
10^7 => 1895683502 -- 4.81 secs
10^8 => 23053423296 -- 47.25 secs

--}

bestHorse :: Integer
bestHorse = f 10000000000 2602680992096 -- 10^9
  where
    f 1000000000000 acum = acum -- 10^11
    f n acum | gg' n && ff' n = f (n+2) (n+acum)
             | otherwise = f (n+2) acum

    ff' n | n > smallLimit = True
          | otherwise = and [mod i (n-1) /= 0 | i <- smallsMinus1] 
    gg' n = mod two60Minus1 (n-1) == 0

testable = f 2 0 -- == 135411214
  where
    f 1000000 acum = acum
    f n acum | gg' n && ff' n = f (n+2) (n+acum)
             | otherwise = f (n+2) acum

    ff' n | n > smallLimit = True
          | otherwise = and [mod i (n-1) /= 0 | i <- smallsMinus1] 
    gg' n = mod two60Minus1 (n-1) == 0

{--
Factors for 2^60-1:
  [[3,2],[5,2],[7,1],[11,1],[13,1],[31,1],[41,1],[61,1],[151,1],[331,1],[1321,1]]

either:
factorise 1152921504606846975
primeFactors 1152921504606846975
--} 

commonToSmalls = [3,5,7,11,13,31,41,151,331]
distinctTwo60 =  [3,5,7,11,13,31,41,61,151,331,1321]

main = do
  let format = "Test Match 135411214 => %s\nReal Deal: %i\n"
  putStr $ printf format (show testable) bestHorse
