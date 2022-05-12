module LinearBits where
import Prelude hiding (head, tail, length)
import Linear.ListableBits
import Data.Bits

{--
This library treats Bits as if they were Vectors and Matrices.
solution5 solves the WoW lights problem for 5 lights.
--}

solution5 :: Int -> Int
solution5 n = 727834 |> (31 <> n)

solvesBases :: Bool
solvesBases = map solution5 bases == [1, 13, 11, 8]

bases :: [Int]
bases = [24, 28, 14, 7]

class (Listable a) => Vector a where
  (<|>) :: a -> a -> a
  eval :: a -> a

  fill :: Int -> a -> a
  fill 0 v = mempty
  fill n v = cons v $ fill (n-1) v

instance Vector Int where
  eval n = popCount n <|> 1
  (<|>) = (.&.)

class Matrix a where
  (|>) :: a -> a -> a -- <A|v>

instance Matrix Int where
-- TODO: write (|>) as a comonad.
  (|>) m v =
    let full_v = fill (length m) v in
    chunkEvals (m <|> full_v) 0
    where
      chunkEvals 0 i = 0
      chunkEvals n i =
        shiftL (eval.head $ n) i + chunkEvals (tail n) (i+1)
