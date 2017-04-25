module DependentCalculation where

data V a = V [a] deriving (Show, Eq)

f1 = V [1,1,0,0,0,0,0,0]
f2 = V [1,1,1,0,0,0,0,0]
f3 = V [0,1,1,1,0,0,0,0]
f4 = V [0,0,1,1,1,0,0,0]
f5 = V [0,0,0,1,1,1,0,0]
f6 = V [0,0,0,0,1,1,1,0]
f7 = V [0,0,0,0,0,1,1,1]
f8 = V [0,0,0,0,0,0,1,1]

instance (Integral a, Num a) => Num (V a) where
  (+) (V as) (V bs) = V [mod (a + b) 2 | (a, b)<- zip as bs ]

path :: (Integral a, Num a)=> [V a] -> V a -> V a
path [] x = x
path (v:vs) x = path vs (v + x)