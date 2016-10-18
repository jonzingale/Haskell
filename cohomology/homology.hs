module Homology where
import Data.List
{--
Here is an attempt to define some of
the basic operations of homology.
--}

type Index = Int
data Simplex a = S [a] deriving (Show)


-- i suspect I need an fmap for boundary

-- ith_face :: Simplex a -> Simplex a

boundary :: Simplex a -> Simplex (Simplex a)
boundary (S []) = S []
boundary (S xs) = S $ map S [ take_and_drop i xs | i <- [0..length xs - 1] ]
  where
    take_and_drop n list = take n list ++ drop (n+1) list

k3 :: Simplex Int
k3 = S [0,1,2]