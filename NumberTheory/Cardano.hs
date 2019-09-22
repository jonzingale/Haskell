module Cardano where

-- type Args = Float -> Float -> Float -> Float

discrim :: Float -> Float -> Float -> Float -> Float
discrim a b c d = 18*a*b*c*d - 4*b^3*d + b^2*c^2 - 4*a*c^3 -27*a^2*d^2

-- discrim 2 3 (-5) (-7) == 469


{--
Develope a function which given coefficients
returns solutions in radicals via Cardano's Formula.
--}