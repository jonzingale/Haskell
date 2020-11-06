module Heyting where
import Math.NumberTheory.Primes.Factorisation (factorise)
import Data.Numbers.Primes (primeFactors)

{--
Let a∗ = max { b ∈ L : b ∧ a = 0 } be the pseudocomplement of a.
Alternatively, a* is the union of all propositions y which have
nothing in common with a.

We see from the definition that a → b = V { c ∈ H : a ∧ c ≤ b }.
As arbitrary joins of elements need not exist in a lattice,
the existence of an implication is not automatic.

Hmm, how should I represent:
1. bounded lattices?
2. lack of arbitrary joins, wedge sums?
--}

class Heyting h where
  hmeet, hjoin, himplies :: h -> h -> h
  hcomplement :: h -> h
  top, bottom :: h

-- arbitrary joins here.
instance Heyting Int where
  top = undefined
  bottom = 1
  hmeet a b = gcd a b
  hjoin a b = lcm a b
  hcomplement a = foldr hjoin bottom [ z | z <- [1..10], hmeet a z <= bottom]
  -- hcomplement a = himplies a bottom -- first fix himplies
  himplies a b = -- todo: think about the bounds here
    foldr hjoin bottom [ z | z <- [1..b], hmeet z a <= b]

testImplies = himplies 3 (5::Int)

{--
todo:
* monoidal Heyting with bottom as empty
* generalize to any poset?
* causal sets, boolean sub-algebra, ...
--}
