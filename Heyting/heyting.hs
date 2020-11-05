module Heyting where
import Math.NumberTheory.Primes.Factorisation (factorise)
import Data.Numbers.Primes

-- data IntLat = H Int | Infinity

class Arithmetic a where
  divs :: a -> a -> Bool

instance Arithmetic Int where
  divs a b = mod b a == 0

class Heyting h where
  hmeet, hjoin, himplies :: h -> h -> h
  top, bottom :: h
  hneg :: h -> h

instance Heyting Int where
  top = undefined
  bottom = 1
  hmeet a b = gcd a b
  hjoin a b = lcm a b
  himplies a b = -- todo: think about the bounds here
    foldr hjoin 1 [ z | z <- [1..b], hmeet z a <= b]
  hneg = undefined

testImplies = foldr hjoin 1 [1..5::Int]

{--
todo:
* monoidal Heyting with bottom as empty
* generalize to any poset?
* causal sets, boolean sub-algebra, ...
--}

