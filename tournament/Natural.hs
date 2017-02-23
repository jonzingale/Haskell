module Natural where
import Prelude hiding (head, (++), tail, length, push, filter)

data Nat n = N n | BAD deriving (Show, Eq, Ord)

incl :: (Integral a) => a -> Nat a
incl n | n < 0 = BAD
       | otherwise = N n

eval :: (Integral a) => Nat a -> a
eval (N a) = a
eval BAD = -1

instance (Num a, Integral a) => Num (Nat a) where
  x + y | x == BAD || y == BAD = BAD
        | otherwise = incl (eval x + eval y)

  x * y | x == BAD || y == BAD = BAD
        | otherwise = incl (eval x * eval y)

  x - y | x == BAD || y == BAD = BAD
        | otherwise = incl (eval x - eval y)

-- instance (Num a, Integral a) => Integral (Nat a) where
--   div x y | x == BAD || y == BAD = BAD
--           | otherwise = N (mod x y)

type N = Nat Integer