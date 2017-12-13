module StructuralNumbers where

prod :: [Integer] -> [Integer] -> [Integer]
prod ns ms = [ n * m | n<-ns, m<-ms]

diag :: (a -> b) -> a-> (b, b)
diag f ns = (f ns, f ns)

palindrome :: Integer -> Bool
palindrome n = revN n == n

lenN :: Integer -> Integer
lenN 0 = 0
lenN n = 1 + (lenN.div n) 10

catN :: Integer -> Integer -> Integer
catN al bl = al * 10^(lenN bl) + bl

revN :: Integer -> Integer
revN n = ff n 0
  where
    ff 0 es = es
    ff ns es =  ff (div ns 10) (es * 10 + mod ns 10)

euler4 :: Integer
euler4 = let lst = reverse [100..999] in
  head $ dropWhile (not.palindrome) $ prod lst lst

lockstep :: Integer -> Integer -> [(Integer, Integer)]
lockstep 0 _ = []
lockstep _ 0 = []
lockstep n k | n <= k = (n, k) : lockstep n (k - 1)
             | otherwise = (n, k) : lockstep (n - 1) k
