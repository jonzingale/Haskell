module Solution where
import Math.NumberTheory.Primes.Factorisation -- factorise
import Math.NumberTheory.Primes -- primes

{--
Consider the fraction, n/d, where n and d are positive integers.
If n<d and HCF(n,d)=1, it is called a reduced proper fraction.

If we list the set of reduced proper fractions for d ≤ 8
 in ascending order of size, we get:

1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 
1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8

It can be seen that 2/5 is the fraction immediately to the left of 3/7.

By listing the set of reduced proper fractions for d ≤ 1,000,000
in ascending order of size, find the numerator of the fraction
immediately to the left of 3/7.
--}

euler71 = g 11 (5,12)  (10^6)
  where
    g k (a,b) n | k <= n = g (k+7) (a+3, b+7) n
                | otherwise = a-3

{--
Finding from Ivan Niven's 'Diophantine Approximations' the theorem of Hurwitz, 
it is clear that any n/d to the left of 3/7 must satisfy 3d−7n=1. 
Looking at the term before 3/7 in each Farey sequence, I noticed that 
starting from the 11th Farey sequence each numerator increases by 3 and each 
denominator by 7, every 7 sequences.  More generally, any Farey number n/d to 
the left of a/b can be produced inductively by considering b sequences at (x,y) 
and then incrementing (x+a, y+b).

Of course looking at it now, It would be much simpler for any (n,d)
to let j=10^6/d and consider j ∗ n − 1. 
--}