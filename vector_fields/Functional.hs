module Functional where

data Functional t = T (t -> t -> t -> t)

instance (Ord t, Num t) => Num (Functional t) where
  (+) (T f) (T g) = T $ \x y z -> f x y z + g x y z
  (-) (T f) (T g) = T $ \x y z -> f x y z - g x y z
  (*) (T f) (T g) = T $ \x y z -> f x y z * g x y z
  abs lf = lf * lf

pr1 (T f) = (\f i -> f i 0 0) f
pr2 (T f) = (\f i -> f 0 i 0) f
pr3 (T f) = (\f i -> f 0 0 i) f
