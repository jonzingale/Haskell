module Solution where
import Math.NumberTheory.Primes.Factorisation -- factorise
import Math.NumberTheory.Primes
{--
Euler's Totient function, φ(n) [sometimes called the phi function],
is used to determine the number of positive numbers less than or equal
to n which are relatively prime to n. For example, as 1, 2, 4, 5, 7, and 8,
are all less than nine and relatively prime to nine, φ(9)=6.
The number 1 is considered to be relatively prime to every positive number, 
so φ(1)=1.

Interestingly, φ(87109)=79180, and it can be seen that
87109 is a permutation of 79180.

Find the value of n, 1 < n < 107, for which φ(n) is a permutation of n
and the ratio n/φ(n) produces a minimum.
--}


{-- METHOD 1
The strategy employed here is to keep account of the last best ratio
and to count on sorting on digits being faster than producing permutations.
Additionally, it seemed worth the risk to only consider odd numbers as
the number of factors increases significantly with even numbers, increasing
the n / phi n ratio.
--}
euler70 = f 23 (5/3) 21
  where
    f j r n | j > 10^7 = n
            | minCond j r = f (j+2) (ratio j) j
            | otherwise = f (j+2) r n
    minCond i t = ratio i < t && (qsort.listify.phi) i == (qsort.listify) i
    ratio i = fromIntegral i / (fromIntegral.phi) i
    phi n = product [ a^(b-1) * (a-1) | (a,b) <- factorise n]

qsort [] = []
qsort [a] = [a] 
qsort (a:as) = (qsort.smaller) (a:as) ++ [a] ++ (qsort.larger) (a:as)
  where
    smaller (a:as) = filter (\x-> x < a ) as
    larger  (a:as) = filter (\x-> x >= a) as

listify n = ff n
  where
    ff 0 = []
    ff n = mod n 10 : (ff.div n) 10

{-- METHOD 2
Products of two primes will likely have the largest totients and thus
the smallest n/ phi ratio. Get all products of two primes up to around
the sqrt of 10^7 and their totients. calculate perms via qsort.
--}

euler70' = f primePairs 2 1
  where
    f ((p,q):ps) r n | (p*q) > 10^7 = n
                     | minCond p q r && permCond p q = f ps (ratio p q) (p*q)
                     | otherwise = f ps r n

    ratio p q = fromIntegral (p*q) / (fromIntegral.phi p) q
    permCond p q = (qsort.listify) (phi p q) == (qsort.listify) (p*q)
    minCond p q r = ratio p q < r

    primePairs = [(p,q) | p<-primes', q<-primes']
    primes' = takeWhile (< 4000) primes
    phi p q = p*q-p-q+1


