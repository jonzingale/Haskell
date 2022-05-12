module Linear.BitHelpers where
import Linear.ListableBits

-- toBitArray m => 727834
toBitArray :: Listable a => [a] -> a
toBitArray bs = foldr cons mempty bs

toBin :: Int -> [Int]
toBin 0 = [0]
toBin n = reverse (helper n)
  where
    helper 0 = []
    helper n | n `mod` 2 == 1 = 1 : helper (n `div` 2)
             | n `mod` 2 == 0 = 0 : helper (n `div` 2)
