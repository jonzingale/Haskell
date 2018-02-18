{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module LinearAlgebra where
import Functional
import Vector

{--
  Given any pair, derive the other.

  given φ & f: pullback
  given f & φ: invertible φ (section)
  given f & g: linear map (coalgebra?)

       φ
    A ---> B
     \    /
    f \  / g
       R

consider a bra and ket notation:
import Text.Printf
instance Show Linear where
  show (L a b) = printf "<%f|%f>" a b

--}

scalar = S 5
v1 = V3 2 3 5
v3 = V3 1 2 1
f1 = T $ \i j k-> 2*i + 3*j + 5*k
idf = T $ \i j k-> i + j + k

test1 = (tr_dn.tr_up) v3
test2 = eval (tr_up v3) v3 == eval idf (v3 * v3)

tr_up :: Num v => Vector v -> Functional v
tr_up (V3 x y z) = T $ \i j k -> i*x + j*y + k*z

tr_dn :: Num v => Functional v -> Vector v
tr_dn f = V3 (pr1 f 1) (pr2 f 1) (pr3 f 1)

instance Num v => Linear (Functional v) (Vector v) where
  eval f (V3 x y z) = S $ pr1 f x + pr2 f y + pr3 f z

-- perhaps <|> when there is a Riesz representation?
-- when an inner product space, there is an iso v↦⟨v,−⟩.
-- Think Gauge.
class Linear f v | v -> f where
  eval :: f -> v -> v
