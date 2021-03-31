module UnpackedBwt (burrows_wheeler) where
import System.Random (mkStdGen, randomRs)
import Data.List (sort) -- merge sort

{--
An attempt to unpack the details of an optimized Burrow-Wheeler transform.
--}

text = "banana_bandana"

burrows_wheeler :: String -> String
burrows_wheeler xs = rsort.(take $ length xs+1).rotate $ '|' : xs
  where rotate (x:xs) = (x:xs) : rotate (xs ++ [x])

-- modified qsort with randomized input
rsort :: [String] -> String
rsort = modSort.shuffle
  where
    modSort [] = []
    modSort [x] = [last x]
    modSort (x:xs) = modSort (less x xs) ++ [last x] ++ modSort (more x xs)
    less a bs = filter (<= a) bs
    more a bs = filter (> a) bs

-- probabilistic key shuffle based on birthday problem
shuffle :: Ord a => [a] -> [a]
shuffle xs = (snd.unzip.sort.zip ((pmonicrandos.length) xs)) xs
  where
    pmonicrandos bs = take bs $ (spitRandos.thrufloat pred_tol) bs
    pred_tol r = (r - r^2) / (2 * log 0.5) -- predicted tolerance
    spitRandos n = randomRs (0, n) $ mkStdGen 42

thrufloat :: (RealFrac a, Integral b) => (a -> a) -> b -> b
thrufloat f = floor.f.fromIntegral
