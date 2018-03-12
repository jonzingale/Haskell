module AlecsHaskell where

euler1 :: Integer
euler1 = 3 * tri 333 + 5 * tri 200 - 15 * tri 66 - 1000
  where
    tri n = div (n^2 + n) 2

euler2 :: Integer
euler2 = sum [x | x <- [1..999], mod x 3 == 0 || mod x 5 == 0]

euler3 :: Integer
euler3 = sum $ not_three _5s ++ _3s
  where
    _3s = filter (\x -> mod x 3 == 0) [1..999]
    _5s = filter (\x -> mod x 5 == 0) [1..999]
    not_three = filter (\x -> mod x 3 /= 0)

euler4 :: Integer
euler4 = threes + fives_not_three
  where
    threes = f 0
      where
        f 999 = 999
        f n = n + f (n+3)

    fives_not_three = f 0 0
      where
        f 1000 _ = 0
        f n 0 = f (n+5) 1
        f n 1 = n + f (n+5) 2
        f n 2 = n + f (n+5) 0
