module Integration where
import ComplexVector
import Complex

type LinearT v = Vector v -> Vector v
type Integration v = LinearT v -> Vector v -> Vector v

delT :: (Fractional v, Floating v, Num v) => Vector v
delT = S 0.01 -- hmm

pt = V3 (C 1 0) (C 1 0) (C 0 0)

pendulum :: Num v => LinearT v
pendulum (V3 a b c) = V3 (-b) a 0

euler :: (Fractional v, Floating v, Comp v, Num v) => Integration v
euler f v = v + f v * delT 

improvedEuler :: (Fractional v, Floating v, Comp v, Num v) => Integration v
improvedEuler f v = let ds = euler f v in
                    let dds = euler f ds in
                    fmap (* 0.5) (ds + dds)

-- This needs to be completed.
rungeKutta :: (Fractional v, Floating v, Comp v, Num v) => Integration v
rungeKutta f v = let k1 = delT * f v in
                 let k2 = delT * f (v + k1 * delT) in
                 k1 + k2