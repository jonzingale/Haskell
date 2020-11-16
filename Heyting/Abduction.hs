module Abduction where

{--
This module exists to flesh out what is meant by
abduction and under what circumstances it is valid.

let a 𝜀 Zp be verbs and w :: Zp -> Bool be a function
which interprets as '() like a duck',ie., each a 𝜀 Zp
indexes a verb like walks, talks, etc.

Bird is white ^ Swans are white => Bird is a Swan.

overfitting reduces the class of possible swans by
ruling out each *this bird*.

Lastly, each a 𝜀 Zp can be interpreted as a Kripke frame,
with each member of Zp a possible world where p is prime.
For typical composites p, p is possibly prime.
For Carmichaels and actual primes, p is necessarily prime.
--}

witness :: Int -> Int -> Bool
witness p a = a^(p-1) `mod` p == 1

-- Indria makes small errors
indriaDel e p = witness p (e + a)

-- Indria is limited
indriaLim l p = [ witness p a | a <- [1..l]]


{--
Induction:

Bird is white ^ Bird is a Swan => Swans are white.

over-fitting reduces the class of possible Swans by
ruling out each *other bird*.
--}