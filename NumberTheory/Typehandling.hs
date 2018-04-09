module Typehandling where

-----short hands
type N = Integer
inv x = 1/x
half = 0.5

--typehandling
thrufloat :: (RealFrac a, Integral b) => (a->a) -> b -> b
thrufloat f n = (floor.f.fromIntegral) n