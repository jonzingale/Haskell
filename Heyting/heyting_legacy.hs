module Heyting where
import Math.NumberTheory.Primes.Factorisation (factorise)
import Data.Numbers.Primes (primeFactors)
import qualified Data.Set as S

{--
Let a∗ = max { b ∈ L : b ∧ a = 0 } be the pseudocomplement of a.
Alternatively, a* is the union of all propositions y which have
nothing in common with a.

We see from the definition that a → b = V { c ∈ H : a ∧ c ≤ b }.
As arbitrary joins of elements need not exist in a lattice,
the existence of an implication is not automatic.

Hmm, how should I represent:
1. bounded lattices?
2. lack of arbitrary joins, wedge sums? dla?
--}

data Tree a = Leaf a | Tree [Tree a] a [Tree a] deriving (Show, Eq)

class Heyting h where
  hmeet, hjoin, himplies :: h -> h -> h
  hcomplement :: h -> h -> h
  top, bottom :: h -> h

instance Ord a => Heyting (Tree a) where
  bottom (Tree a b c) = Leaf b
  bottom (Leaf a) = Leaf a
  top = undefined

  hmeet (Tree a b c) v w = undefined
