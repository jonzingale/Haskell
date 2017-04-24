module FunctionalExperiment where

data Fun t s r b = T (t -> s -> r -> b)

newtype L a = Fun a

f1 :: L (Integer -> Integer -> Integer -> Integer)
f1 = Fun (\i j k -> i+j+k)

instance (Ord t, Ord s, Ord r, Ord b, Num b, Num t, Num s, Num r) => Num (Fun t s r b) where
  (+) (Fun f) (Fun g) = Fun $ \x y z -> f x y z + g x y z

instance Functor (Fun t s r) where
  fmap f (T g) = T $ (\i j -> (f.(g i j)))
