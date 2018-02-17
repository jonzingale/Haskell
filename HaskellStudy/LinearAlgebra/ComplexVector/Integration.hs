module Integration where
import ComplexVector
import Complex

type LinearT v = Vector v -> Vector v

delT :: (Fractional v, Floating v, Num v) => Vector v
delT = V3 0.01 0.01 0.01

pt = V3 (C 1 0) (C 0 0) (C 0 0)

pendulum :: Num v => Vector v -> Vector v
pendulum (V3 a b c) = V3 (-b) a 0

euler :: (Fractional v, Floating v, Comp v, Num v) => LinearT v -> Vector v -> Vector v
euler f v = v + f v * delT 

improvedEuler :: (Fractional v, Floating v, Comp v, Num v) => LinearT v -> Vector v -> Vector v
improvedEuler f v = let ds = euler f v in
                    let dds = euler f ds in
                    fmap (* 0.5) (ds + dds)
