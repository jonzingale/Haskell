module Solution where

{--
The following iterative sequence is defined for the set of positive integers:

n → n/2 (n is even)
n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:

13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
It can be seen that this sequence (starting at 13 and finishing at 1)
contains 10 terms. Although it has not been proved yet (Collatz Problem),
it is thought that all starting numbers finish at 1.

Which starting number, under one million, produces the longest chain?

NOTE: Once the chain starts the terms are allowed to go above one million.
--}


euler14 = f 1 (1,1)
  where
    f n (a,k) | n > 10^6 = a
              | collatz n > k = f (n+1) (n, collatz n)
              | otherwise = f (n+1) (a, k)

collatz n = f n 1
  where
    f 1 k = k
    f n k | even n = f (div n 2) (k+1)
          | otherwise = f (3*n+1) (k+1)

main = print euler14
