module SortsShuffles where
import System.Random (mkStdGen, randoms)
import Data.List (sort)

seedShuffle :: Ord a => Int -> [a] -> [a]
seedShuffle n xs = snd.unzip.sort.zip (randos n) $ xs 
  where randos n = (randoms $ mkStdGen n)::[Int]

shuffle :: Ord a => [a] -> [a]
shuffle xs = snd.unzip.sort.zip randos $ xs 
  where randos = (randoms $ mkStdGen 23)::[Int]

bubbleSort :: Ord a => [a] -> [a]
bubbleSort ks = bs 0 ks
  where
    bswap i ks = take i ks ++ [ks !! (i+1)] ++ [ks !! i] ++ drop (i+2) ks

    bs i ks | i == length ks - 1 = ks
            | ks !! i <= ks !! (i+1) = bs (i+1) ks
            | otherwise = bs 0 (bswap i ks)

partialBubbleSort :: Ord a => Int -> [a] -> [a]
partialBubbleSort l ks = bs l 0 ks
  where
    bswap i ks = take i ks ++ [ks !! (i+1)] ++ [ks !! i] ++ drop (i+2) ks

    bs l i ks | i == length ks - 1 || l == 0 = ks
              | ks !! i <= ks !! (i+1) = bs l (i+1) ks
              | otherwise = bs (l-1) 0 (bswap i ks)

testPartialBS = do
  let ary = shuffle [0..100]
  let those = iterate (partialBubbleSort 10) ary
  putStr.unlines.map show $ those