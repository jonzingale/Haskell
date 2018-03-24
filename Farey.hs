-- The nth farey sequence is a list of all fractions
-- between zero and with numerator at most n.
-- fractions magically appear in order and reduced. 

farey(0) = []
farey(1) = [(0,1),(1,1)]
farey(n) = boost (n,farey(n-1))

boost (n,[(a,b)]) = [(a,b)]
boost (n,((a,b):(c,d):terms)) | b + d == n = 
                                (a,b) : (a+c,b+d) : boost (n, (c,d):terms )
                              | otherwise =
                                (a,b) : boost (n, (c,d):terms )

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

euler(n,d)=n*div(10^6,d)−1
--}

leftFarey nth (n,d) = 
  let num = n * div nth d - 1 in
  let denom = div (1 + num * d) n in
  (num, denom)
