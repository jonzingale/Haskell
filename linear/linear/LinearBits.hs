module LinearBits where
import qualified Linear.ListableBits as L
import Data.Bits

{--
// inv(l4)*l54
ls =
  [[1, 0, 1, 1, 0], // 22
   [0, 0, 1, 1, 0], // 6
   [1, 1, 0, 0, 0], // 24
   [1, 1, 0, 1, 0]] // 26
)
// 28
v = [1,1,1,0,0]
// 19
v = [1,0,0,1,1]

// 4
ls(v) = [0,1,0,0]

write matrix multiplication bitwise.
`xor` is addition
`and` is multiplication
`shift` to make arrays

getVector 3 727834::Int => 22
getVector 2 727834::Int => 6
getVector 1 727834::Int => 24
getVector 0 727834::Int => 26

--}

v = 19
m = [26, 24, 6, 22::Int] -- 727834

test :: Bool
test = and [shiftR (shiftL x 2) 2 == x | x <- [0..100::Int]]

-- toBitArray m => 727834
toBitArray :: L.Listable a => [a] -> a
toBitArray bs = foldr L.cons mempty bs

-- getVector 2 (toBitArray m) => 6
getVector :: L.Listable a => Int -> a -> a
getVector n bs = bs L.!! n

class Vector a where
  (!+) :: a -> a -> a
  (<|>) :: a -> a -> a

instance Vector Int where
  (!+)  = (<>)
  (<|>) = (.&.)

class Matrix a where
  (!*) :: a -> a -> a

instance Matrix Int where
  (!*) m n = undefined
