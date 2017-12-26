module Challenge102 where
import Triangles

euler102 = head triangles

ex = T (V 448 617) (V (-988) 0) (V (-103) (-504)) -- has a zero
tri = T (V 4 1) (V (-3) (-5)) (V (-3) 6)
zero = V 0 0
pt = V 2 4

data Vect = V Integer Integer
data Triangle = T Vect Vect Vect
type Point = Vect

(+|), (-|) :: Vect -> Vect -> Vect
V a b +| V c d = V (a+c) (b+d)
V a b -| V c d = V (a-c) (b-d)

innerP :: Vect -> Vect -> Integer
innerP (V a b) (V c d) = a*c + b*c

orth :: Vect -> Vect
orth (V x y) = V (-y) x

in_region :: Triangle -> Vect -> Bool
in_region (T a b c) pt =
  let acute = if condo (b -| a) (a -| c) then and else or in
  acute [condo pt (b -| a), condo pt (a -| c)]
  where
    condo v w = innerP v (orth w) >= 0

rotateT :: Triangle -> [Triangle]
rotateT (T a b c) = (T a b c) : rotateT (T b c a)


