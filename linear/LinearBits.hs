module LinearBits where
import Data.Bits

{--
How linear are bitwise operators?
Which if any are and in what way?

f a + f b = f (a + b)

in particular...
𝑓(𝑣,𝑣) = 𝑓(𝑣,0) + 𝑓(0,𝑣) = 2𝑓(𝑣,0) = 0

Reference:
https://math.stackexchange.com/questions/590966/why-do-xor-and-other-operators-on-binary-variables-qualify-as-linear

--}

-- bitwise addition
(.+) :: Int -> Int -> Int
(.+) a 0 = a
(.+) a b = (.+) (xor a b) (shift (a .&. b) 1)

ff :: Int -> Int
ff = xor 2

test1 :: (Int -> Int -> Int) -> Bool
test1 f = all (== True)
  [f v v == 0 | v <- [0..15]]

test2 :: (Int -> Int) -> Bool
test2 f = all (== True)
  [g (f x + f y) == g (f (x + y)) | x <- [0..15], y <- [0..15]]
  where
    g t = mod t 16

test2' :: (Int -> Int) -> [(Int, Int)]
test2' f =
  [(g (f x + f y), (g.f) (x + y)) | x <- [0..15], y <- [0..15]]
  where
    g t = mod t 16

-- test3 (.&. 4) => True
test3 :: (Int -> Int) -> Bool
test3 f = all (== True)
  [f x `xor` f y == f (x `xor` y) | x <- [0..15], y <- [0..15]]
