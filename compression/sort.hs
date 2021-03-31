module Sort (msort, rsort, shuffle) where
import System.Random

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort (less x xs) ++ [x] ++ qsort (more x xs)
 where
  less a bs = filter (<= a) bs
  more a bs = filter (> a) bs

rsort :: [String] -> String
rsort = modSort.shuffle

-- optimization for Burrows-Wheeler
modSort :: [String] -> String
modSort [] = []
modSort [x] = [last x]
modSort (x:xs) = modSort (less x xs) ++ [last x] ++ modSort (more x xs)
 where
  less a bs = filter (<= a) bs
  more a bs = filter (> a) bs

-- probabilistic key shuffle based on birthday problem
shuffle :: Ord a => [a] -> [a]
shuffle xs = (snd.unzip.msort.zip ((pmonicrandos.length) xs)) xs
  where
    pmonicrandos bs = take bs $ (spitRandos.thrufloat pred_tol) bs
    pred_tol r = (r - r^2) / (2 * log 0.5) -- predicted tolerance
    spitRandos n = randomRs (0, n) $ mkStdGen 42

thrufloat :: (RealFrac a, Integral b) => (a -> a) -> b -> b
thrufloat f = floor.f.fromIntegral

-- sort implementation from Data.List
msort :: Ord a => [a] -> [a]
msort = sortBy compare

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy cmp = mergeAll . sequences
  where
    sequences (a:b:xs)
      | a `cmp` b == GT = descending b [a]  xs
      | otherwise       = ascending  b (a:) xs
    sequences xs = [xs]

    descending a as (b:bs)
      | a `cmp` b == GT = descending b (a:as) bs
    descending a as bs  = (a:as): sequences bs

    ascending a as (b:bs)
      | a `cmp` b /= GT = ascending b (\ys -> as (a:ys)) bs
    ascending a as bs   = let !x = as [a]
                          in x : sequences bs

    mergeAll [x] = x
    mergeAll xs  = mergeAll (mergePairs xs)

    mergePairs (a:b:xs) = let !x = merge a b
                          in x : mergePairs xs
    mergePairs xs       = xs

    merge as@(a:as') bs@(b:bs')
      | a `cmp` b == GT = b:merge as  bs'
      | otherwise       = a:merge as' bs
    merge [] bs         = bs
    merge as []         = as
    