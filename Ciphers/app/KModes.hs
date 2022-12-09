module KModes where
import Data.Bits -- popCount
import Data.List

{--
setBit . clearBit = id
popCount is evalZ
--}

centroids :: [Int]
-- 00000, 01111, 11111
centroids = [0, 15, 31]

type Vect = Int
type Mode = Vect
type Collection = [Vect]

evalZ :: Bits b => b -> Int
evalZ b = popCount b

-- Hamming
evalF :: Bits b => b -> b -> Int
evalF v = evalZ . xor v

evalFs :: Bits b => b -> [b] -> [Int]
evalFs v = map (evalF v)

-- vectors MUST be of equal and finite length
mode :: (Bits b, Ord b, Num b) => [b] -> [b]
mode xs =
  -- let size = 1 + (div (length xs) 2) :: Int in
  let size = 5 :: Int in
  (map (f size)) . transposeB $ xs
  where
    -- bias is toward zero in tiebrake
    f s v | 2 * evalZ v > s  = 1
          | otherwise = 0

-- Todo: Write this better
transposeB :: (Num b, Bits b, Ord b) => [b] -> [b]
transposeB vs =
  let len = length vs in -- 4
  let bins = [[res j v| v <- vs] | j <- [0..9]] in
  reverse . map (toBinInt len) $ bins
  where
    toBinInt l v = sum . zipWith (*) v $ [ 2^(l-z) | z <- [1..]]
    res i v
      | v .&. 2^i > 0 = 1
      | otherwise = 0
