module StructuralNumbers where

prod :: [Integer] -> [Integer] -> [Integer]
prod ns ms = [ n * m | n<-ns, m<-ms]

palindrome :: Integer -> Bool
palindrome n = revN n == n

lenN :: Integer -> Integer
lenN 0 = 0
lenN n = 1 + (lenN.div n) 10

catN :: Integer -> Integer -> Integer
catN al bl = al * 10^(lenN bl) + bl

revN :: Integer -> Integer
revN 0 = 0
revN ns = catN (mod ns 10) $ revN $ div ns 10

revM :: Integer -> Integer
revM n = f n 0
  where
    f 0 es = es
    f ns es =  (f.div ns) 10 $ es * 10 + mod ns 10

euler4 = let lst = reverse [100..999] in
  head $ dropWhile (not.palindrome) $ prod lst lst

palN n k | (n * k) == revN (n * k) = (n * k)
         | otherwise = palN n (k - 1)