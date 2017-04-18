module LinearAlgebra where
{--
  d
M -> N   given d & f: pullback
f\ /g    given f & d: invertible d (section)
  r      give f & g: linear map (coalgebra?)
--}

data Vect a = V [a] | Lf [a -> a] | Zero | Bad

ff = Lf [\i -> 2*i, \j -> 3*j, \k -> 5*k]
vv = V [1, 2, 3]
ww = V [-1, 0, 1]

uG (V vs) = vs -- uG $ ff <|> vv <|> ww

instance Num a => Linear (Vect a) where
  (<|>) (Lf fs) (V vs) = V [ f x | (f, x) <- zip fs vs]
  (<|>) (V vs) (V ws) = tr (V vs) <|> V ws
  tr (V vs) = Lf [ (* v) | v <- vs]
  tr (Lf fs) = V [ f 1 | f <- fs]

instance (Num a, Eq a) => Num (Vect a) where
  (+) (V vs) (V ws) = V [ v + w | (v, w) <- zip vs ws]
  (+) (Lf fs) (Lf gs) = Lf [ \x -> f x + g x | (f, g) <- zip fs gs]
  -- test (+) with an identity using tr.
  (*) (V [v]) (V ws) = V $ map (* v) ws
  (*) (V vs) (V ws) = V [ v * w | (v, w) <- zip vs ws]
  (*) (Lf fs) (Lf gs) = Lf [ \x -> f x * g x | (f, g) <- zip fs gs]
  -- is (*) composition? f <|> g == f * g
  (-) (V vs) (V ws) = V [ v - w | (v, w) <- zip vs ws]
  fromInteger c = V [fromIntegral c]
  abs (V vs) =  tr (V vs) <|> V vs
  abs (Lf fs) = (Lf fs) <|> tr (Lf fs)
  signum (V vs) | abs (V vs) == (V vs) = 1
                | otherwise = -1

class Linear v where
  (<|>) :: v -> v -> v
  tr :: v -> v

-- in preparation for a monad distinguishing Rank
zipp :: Vect t -> Vect t1 -> Vect (t, t1)
zipp (V vs) (V ws) | length vs == length ws = V $ zip vs ws
                   | otherwise = Bad

--- Founding Classes
instance (Eq a, Num a) => Eq (Vect a) where
  (==) (Lf fs) (Lf gs) = tr (Lf fs) == tr (Lf gs)
  (==) (V as) (V bs) = as == bs
