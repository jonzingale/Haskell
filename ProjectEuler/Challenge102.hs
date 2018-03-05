module Challenge102 where
import Triangles
import System.Random

-- Prelude> 34491/23568 -- haskell
-- 1.4634674134419552
-- Prelude> 795058/547350 -- rubyprocessing
-- 1.4525586918790536
-- Prelude> 1338107/656561 --ruby
-- 2.038054346816214 


{--
Four ideas for finding a zero in a triangle.
* GeoCoding
* 4 Quadrants
* Barycentric
* Ray Crossings
--}

euler102 = head triangles

-- GeoCoding

ex = T (V 448 617) (V (-988) 0) (V (-103) (-504)) -- has a zero
badone = T (V 0 4) (V (-1) 1) (V 1 1)-- shouldn't have a zero

tri = T (V 4 1) (V (-3) (-5)) (V (-3) 6)
zero = V 0 0
pt = V 2 4

data Vect = V Integer Integer deriving Show
data Triangle = T Vect Vect Vect deriving Show
type Point = Vect

(+|), (-|) :: Vect -> Vect -> Vect
V a b +| V c d = V (a+c) (b+d)
V a b -| V c d = V (a-c) (b-d)

innerP :: Vect -> Vect -> Integer
innerP (V a b) (V c d) = a*c + b*d

orth :: Vect -> Vect
orth (V x y) = V (-y) x

in_region :: Vect -> Triangle -> Bool
in_region pt (T a b c) =
  let acute | condo (b -| a) (a -| c) = and
            | otherwise = or in
  acute [condo pt (b -| a), condo pt (a -| c)]
  where
    condo v w = innerP v (orth w) >= 0

rotateT :: Triangle -> [Triangle]
rotateT (T a b c) = (T a b c) : rotateT (T b c a)

spinT :: Triangle -> [Bool]
spinT tri = take 3 $ map (in_region zero) $ rotateT tri
