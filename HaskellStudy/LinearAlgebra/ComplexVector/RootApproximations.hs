module RootApproximations where

-- newton-ralphson method.

type Polynomial = [Double]
type Steps = Integer

pp, ppp :: Polynomial
pp = [1, 0, -4] -- x^2 - 4
ppp = [20, 0, 3, -5] -- 20x^3 + 3x - 5

newton :: Polynomial -> Steps -> Double -> Double
newton f n x = newtralph f (del f) n x 
  where
    newtralph _ _  0 x = x
    newtralph f f' n x = newtralph f f' (n-1) $ x - (eval f x) / (eval f' x)

eval :: Polynomial -> Double -> Double
eval [] x = 0
eval (f:fs) x = f * x ^ (fromIntegral.length) fs + eval fs x

del :: Polynomial -> Polynomial
del [f] = []
del (f:fs) = f * (fromIntegral.length) fs : del fs