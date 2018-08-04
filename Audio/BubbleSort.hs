module BubbleSort where
import System.Random (mkStdGen, randoms)
import Data.Int (Int32)
import Data.List (sort)

{--
ByteString or Unboxed Vectors for speed?
--}

data KeyedSample = KS {key :: Int, sample :: [Int32]} deriving Eq

instance Show KeyedSample where
  show (KS i x) = "KS " ++ show i ++ " " ++ (show.head) x ++ " "

instance Ord KeyedSample where
  (<=) (KS i x) (KS j z) = i <= j
  (>=) (KS i x) (KS j z) = i >= j

unkey :: [KeyedSample] -> [[Int32]]
unkey ks = map sample ks

shuffle :: Ord a => [a] -> [a]
shuffle xs = snd.unzip.sort.zip randos $ xs 
  where randos = (randoms $ mkStdGen 23)::[Int]

toKeyedSamples :: [[Int32]] -> [KeyedSample]
toKeyedSamples xs = [ KS i x | (i, x) <- zip [0..] xs ]

bubbleSort :: Ord a => [a] -> [a] -- 37 secs for 48k samples
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

-- nested probabilistic sort.

