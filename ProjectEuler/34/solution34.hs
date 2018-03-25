{--
145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

Find the sum of all numbers which are equal to the
sum of the factorial of their digits.

Note: as 1! = 1 and 2! = 2 are not sums they are not included.
--}

listify n = ff n
  where
    ff 0 = []
    ff n = (ff (div n 10)) ++ [mod n 10]

numbify ls = ff.reverse $ ls
  where
    ff [] = 0
    ff (n:ns) = n + ff (map (*10) ns)

fact :: Integer -> Integer
fact n = foldr (*) 1 [1..n]

sumFacts = sum.map fact

-- 10^7 156.73 secs

main = do
  let euler34 = sum [n | n<-[3..10^8], (sumFacts.listify) n == n]
  print euler34