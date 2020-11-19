module HeytingFactors where
-- REF: https://www.wikiwand.com/en/Heyting_algebra

-- todo: Parameterize this type
data NumLattice = NL Int Int | Fail deriving (Show, Eq)

factors :: Int -> [Int]
factors n = [ x | x <- [1..n], mod n x == 0 ]

class Heyting h where
  implies :: h -> h -> h
  hAnd :: h -> h -> h
  hOr :: h -> h -> h
  hNot :: h -> h

instance Heyting NumLattice where
  hAnd (NL n a) (NL m b)
    | n == m = NL n (gcd a b)
    | otherwise = Fail

  hOr (NL n a) (NL m b)
    | n == m = NL n (lcm a b)
    | otherwise = Fail

  implies (NL n a) (NL m b)
    | n == m = NL n $ foldr lcm 1 [ z | z <- factors n, mod b (gcd z a) == 0 ]
    | otherwise = Fail

  hNot (NL n a) = implies (NL n a) (NL n 1)

testImplies30 = implies (NL 60 4) (NL 60 6) == NL 60 30
testImplies20 = implies (NL 60 6) (NL 60 4) == NL 60 20
testNot = (NL 60 6) /= (hNot.hNot) (NL 60 6)