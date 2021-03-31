module UnpackedBwt (burrows_wheeler) where
import Data.List (sort) -- merge sort
import System.Random

{--
An attempt to unpack the details of an optimized Burrow-Wheeler transform.
--}

text = "banana_bandana"

burrows_wheeler :: String -> String
burrows_wheeler xs = rsort.(take $ length xs+1).rotate $ '|' : xs
  where rotate (x:xs) = (x:xs) : rotate (xs ++ [x])

-- modified qsort with randomized input
rsort :: [String] -> String
rsort = modSort.knuffle
  where
    modSort [] = []
    modSort [x] = [last x]
    modSort (x:xs) = modSort (less x xs) ++ [last x] ++ modSort (more x xs)
    less a bs = filter (<= a) bs
    more a bs = filter (> a) bs

-- key shuffle based on birthday problem
knuffle :: Ord a => [a] -> [a]
knuffle xs = (snd.unzip.sort.zip ((pmonicrandos.length) xs)) xs
  where
    pmonicrandos bs = take bs $ (spitRandos.thrufloat pred_tol) bs
    spitRandos n = randomRs (0, n) $ mkStdGen 42
    pred_tol r = (r - r^2) / (2 * log 0.5) -- predicted tolerance

thrufloat :: (RealFrac a, Integral b) => (a -> a) -> b -> b
thrufloat f = floor.f.fromIntegral
