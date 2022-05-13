module ListableBits where
import Prelude hiding (head, tail, length, take, drop, (!!), (++))
import Data.Bits

{--
Bits of fixed length as lists
--}

-- fix vector length
v_len :: Int
v_len = 5

class (Monoid a, Eq a) => Listable a where
  cons:: a -> a -> a
  tail :: a -> a

  head :: a -> a
  head bs = cons mempty (tail bs) <> bs

  take :: Int -> a -> a
  take 0 bs = mempty
  take n bs = cons (head bs) $ take (n-1) $ tail bs

  drop :: Int -> a -> a
  drop 0 bs = bs
  drop n bs = drop (n-1) $ tail bs

  (!!) :: a -> Int -> a
  (!!) bs n = head.drop n $ bs

  (++) :: a -> a -> a
  (++) = cons

  length :: a -> Int
  length n
    | n == mempty = 0
    | otherwise = 1 + length (tail n)

instance Semigroup Int where
  (<>) = xor

instance Monoid Int where
  mempty = 0

instance Listable Int where
  cons b bs = shiftL bs v_len <> b
  tail = flip shiftR v_len
