module LinearBits where
import Prelude hiding (head, tail, length)
import Linear.ListableBits
import Linear.BitHelpers
import Data.Bits

{--
This library treats Bits as if they were Vectors and Matrices.
solution5 solves the WoW lights problem for 5 lights.

// inv(l4)*l54
ls = 727834
  [[1, 0, 1, 1, 0], // 22
   [0, 0, 1, 1, 0], // 6
   [1, 1, 0, 0, 0], // 24
   [1, 1, 0, 1, 0]] // 26

22 <|> 19::Int => 18
[1, 0, 1, 1, 0] <|> [1, 0, 0, 1, 1] => [1, 0, 0, 1, 0]

map (eval.((<|>) 19)) m => [0,1,1,0] => 6

(fill 4 (19::Int)) <|> 727834 => 592402

--}

v = 19
m = [26, 24, 6, 22::Int] -- 727834
bases = [24, 28, 14, 7::Int]

-- solves 5 lights problem
solution5 :: Int -> Int
solution5 n = 727834 |> (31 !+ n)

solveBases = map solution5 bases == [1, 13, 11, 8]

class (Listable a) => Vector a where
  (<|>) :: a -> a -> a
  eval :: a -> a

  (!+) :: a -> a -> a
  (!+)  = mappend

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
