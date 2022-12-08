module KModes where
import Data.Bits -- popCount
import Data.List

{--
setBit . clearBit = 1
popCount is evalZ

--}

centroids :: [Int]
-- 00000, 01111, 11111
centroids = [0, 15, 31]

type Vect = [Int]
type Mode = Vect
type Collection = [Vect]

dallas :: Int
dallas = 31

evalZ :: Bits b => b -> Int
evalZ b = popCount b

-- Hamming
evalF :: Bits b => b -> b -> Int
evalF v = evalZ . xor v

evalFs :: Bits b => b -> [b] -> [Int]
evalFs v = map (evalF v)

-- transpose doesn't know about Bits as [Z2]
mode :: (Bits b, Ord b, Num b) => [b] -> [Int]
mode xs = (map f) . transposeB $ xs
  where
    -- bias toward zero in tiebrake
    f v | 2 * evalZ v > bitLen v = 1 -- not getting the length of vector
        | otherwise = 0
    bitLen n = f n 0
      where
        f n a | n - 2^a >= 0 = f n (a+1)
              | otherwise = a

-- Todo: Write this better
-- tranposeB centroids => [1,3,3,3,3]
transposeB :: (Num b, Bits b, Ord b) => [b] -> [b]
transposeB vs =
  let len = length vs in
  [toBinInt len [res len j v| v <- vs] | j <- [0..len + 1]]
  where
    toBinInt l v = sum . zipWith (*) v $ [ 2^(l-z) | z <- [1..]]
    res l i v 
      | v .&. 2^(l-i+1) > 0 = 1
      | otherwise = 0




