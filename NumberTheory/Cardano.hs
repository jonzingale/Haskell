module Cardano where
import Data.Complex

data Cubic = C Float Float Float Float deriving (Show, Eq)
-- data CCubic = CC Complex Complex Complex Complex deriving (Show, Eq)

-- type Args = Float -> Float -> Float -> Float

discrim :: Float -> Float -> Float -> Float -> Float
discrim a b c d = 18*a*b*c*d - 4*b^3*d + b^2*c^2 - 4*a*c^3 -27*a^2*d^2

-- discrim 2 3 (-5) (-7) == 469


{--
Develope a function which given coefficients
returns solutions in radicals via Cardano's Formula.


1) transform polynomial to normal form:
ax^3 + bx^2 + cx + d -> x^3 + px + q

2) substitute to flavor
--}

transform :: Cubic -> Cubic
transform cubic = t.monic $ cubic
  where
    t (C a b c d) = C 1 0 (p a b c d) (q a b c d)
    monic (C a b c d) = C (a/a) (b/a) (c/a) (d/a)
    q _ a b c = 2/27 * a^3 - 1/3 * a*b + c
    p _ a b c = -1/3 * a^2 + b

example = cardano (C 1 0 (-1) 0)

cardano :: Cubic -> String
cardano cubic =
  let (C t t' p q) = transform cubic in
  -- let fstComp = "(-(q/2) + sqrt((q/2)^2 + (p/3)^3 )^1/3" in
  let fstComp = "(" ++ show (-q/2) ++ " + sqrt(" ++ show ((q/2)^2) ++
                " + " ++ show((p/3)^3) ++ "))^1/3" in
  -- let sndComp = "(-(q/2) - sqrt((q/2)^2 + (p/3)^3 )^1/3" in
  let sndComp = "(" ++ show (-q/2) ++ " - sqrt(" ++ show ((q/2)^2) ++
                " + " ++ show((p/3)^3) ++ "))^1/3" in
  fstComp ++ " + " ++ sndComp
