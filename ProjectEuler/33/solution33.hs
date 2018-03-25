{--
The fraction 49/98 is a curious fraction, as an inexperienced mathematician
in attempting to simplify it may incorrectly believe that 49/98 = 4/8,
which is correct, is obtained by cancelling the 9s.

We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

There are exactly four non-trivial examples of this type of fraction,
less than one in value, and containing two digits in
the numerator and denominator.

If the product of these four fractions is given in its lowest common terms,
find the value of the denominator.
--}

module Solution where

listify n = ff n
  where
    ff 0 = []
    ff n = (ff (div n 10)) ++ [mod n 10]

numbify ls = ff.reverse $ ls
  where
    ff [] = 0
    ff (n:ns) = n + ff (map (*10) ns)


-- filterDigit n = filter (== n)

-- ary = [(n,d)| n<-[1..10^2],d<-[1..10^2], f n / f d == fi n / fi d]
--   where
--     f = fi.numbify.listify
--     fi = fromInteger
    -- fil = all (\)

-- test :: [[Integer]]
test = zip (redCond 49) (redCond 98)

farey n = [(a,b)| b<-[1..n], a<-[1..b-1]]

redCond n = map numbify $ reductions <*> lss n
  where
    reductions = (\k-> filter (/= k)) <$> [1..9]
    lss n = [listify n]