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

data BoundedInt = B Int | Infinity deriving (Show, Eq)

instance Ord BoundedInt where
  (<=) (B a) (B b) = a <= b
  (<=) (B a) Infinity = True
  (<=) Infinity Infinity = True
  (>=) = flip (<=)

testImplies = himplies (B 3) (B 5)

class Heyting h where
  hmeet, hjoin, himplies :: h -> h -> h
  hcomplement :: h -> h
  top, bottom :: h

-- arbitrary joins here.
instance Heyting BoundedInt where
  top = Infinity
  bottom = B 1
  hmeet (B a) (B b) = B $ gcd a b
  hjoin (B a) (B b) = B $ lcm a b
  hcomplement a =
    foldr hjoin bottom [ B z | z <- [1..12], hmeet a (B z) <= bottom]
  -- hcomplement a = himplies a bottom -- first fix himplies

  -- todo: think about the better bounds here
  himplies a bb =
    let (B b) = bb in 
    foldr hjoin bottom [ B z | z <- [1..12], hmeet (B z) a <= bb]

data BoundedSet a = BS (S.Set a) | Top deriving (Show)

instance Eq (BoundedSet a) where
  (==) (BS a) (BS b) = (S.size a) == (S.size b)

-- S.fromList :: Ord a => [a] -> S.Set a
-- S.insert :: Ord a => a -> S.Set a -> S.Set a, elems
instance Ord (BoundedSet a) where
  (<=) (BS a) (BS b) = (S.size a) <= (S.size b)
  (<=) (BS a) Top = True
  (<=) Top Top = True
  (>=) = flip (<=)

{--
I need a way to index subsets, (BS z) <- [bottom..top]. The deeper issue is
likely an ontological one. I may need to have a psuedo-lattice in hand first,
ie., know what the full union looks like and how to derive the subsets.
--}

instance Ord a => Heyting (BoundedSet a) where
  top = Top
  bottom = BS S.empty
  hjoin (BS a) (BS b) = BS $ S.intersection a b
  hmeet (BS a) (BS b) = BS $ S.union a b
  himplies a b = undefined
  hcomplement a = undefined

{--
Try with Object in hand? Do I do this monadically?
for complement and implication, I will need to append some
context and work from that.
--}
data Relations a = R [(a, a)] deriving (Show, Eq) -- a -> b

