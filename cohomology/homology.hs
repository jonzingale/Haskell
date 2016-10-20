module Homology where
import Matrix

{--
Here is an attempt to define some of
the basic operations of homology.

think of wrapping simplexes σ^m in
chains (abelian groups).

∑ gi . σi^m <-> <gi...gm>

--}

data Chain a = C [Chain a] | S [a] deriving (Show)

facet :: Num a => Int -> Chain a -> Chain a 
facet n (S xs) = (facet n).del $ (S xs)
facet n (C xs) = xs!!n

instance Functor Chain where
  fmap f chain = case chain of
    S a -> S (map f a)
    C a -> C (map (fmap f) a)

-- determinant as del
-- det :: Matrix -> Int

is_cycle :: (Num a, Eq a) => Chain a -> Bool
is_cycle chain = (f.del) chain == 0
  where
    f (S []) = 0
    f (C []) = 0
    f (S (x:xs)) = x + f (S xs)
    f (C xs) = foldr (+) 0 (map f xs)

del :: Num a => Chain a -> Chain a
del (S []) = C []
del (C xs) = C $ map del xs
del (S xs) = C [ fx i xs| i <- ary ]
  where
    ary = [0..length xs - 1]
    gx n list = map (* (- 1)^n) list
    fx n list = S . gx n $ take n list ++ drop (n+1) list

k3 :: Chain Integer
k3 = S [1,2,3]

k4 :: Chain Integer
k4 = S [1,2,3,4]