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
import qualified Data.Set as S

euler33 = g.f $ [(n,d) | (n,d)<-farey 200, test (n,d)]
  where
    f = takeWhile (\(n,d) -> n<100 && d<100)
    g = (\(ns,ds)-> div (product ds) (product ns)).unzip

listify n = ff n
  where
    ff 0 = []
    ff n = (ff (div n 10)) ++ [mod n 10]

numbify ls = ff.reverse $ ls
  where
    ff [] = 0
    ff (n:ns) = n + ff (map (*10) ns)

farey n = [(a,b)| b<-[1..n], a<-[1..b-1]]

test :: (Integer, Integer) -> Bool
test (n,d) = or $ False : [True | (a,c) <- zipped n d, cond n d a c]
  where
    fi = fromInteger
    cond n d a c = fi n / fi d == a / c && fi n /= a
    zipped n d = (S.toList).(S.fromList).zip (redMask n) $ redMask d

redMask :: Integer -> [Double]
redMask n = map numbify $ reductions <*> lss n
  where
    reductions = (\k-> filter (/= k)) <$> [1.0..9.0]
    lss n = [map fromInteger $ listify n]