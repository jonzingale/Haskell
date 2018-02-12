module ListsAsNumbers where

{--
meta methods:

quit => :q
type => :t
reload => :r
browse standard library => :browse Prelude
show benchmarks => :set +s
--}

ff :: [Integer]
ff = [1,2,3,4,5,6]

gg :: [Integer]
gg = [1..20]

dropten :: [Integer] -> [Integer]
dropten xs = (take 9 xs) ++ (drop 10 xs)

-- Length of a list
len :: [a] -> Integer
len [] = 0
len [x] = 1
len (x:xs) = 1 + (len xs)

hh :: [a] -> a
hh (x:xs) = x

tt :: [a] -> [a]
tt (x:xs) = xs


------------
-- Numbers are Lists

listify :: Integer -> [Integer]
listify 0 = []
listify n = (listify (div n 10)) ++ [mod n 10]

numbify :: [Integer] -> Integer
numbify [] = 0
numbify (x:xs) = x * 10^(len xs) + numbify xs

catN :: Integer -> Integer -> Integer
catN n1 n2 = numbify ((listify n1) ++ (listify n2))

lenN :: Integer -> Integer
lenN n = len (listify n) 


------------
-- Number functions without lists
-- max, min

--take
takeN :: Integer -> Integer -> Integer
takeN n d = div (mod n (10^d)) (10^(d-1))

--length
len2 :: Integer -> Integer
len2 0 = 0
len2 n = 1 + len2 (div n 10)

--concatenate
fastcat :: Integer -> Integer -> Integer
fastcat n1 n2 = (n1 * 10^(len2 n2)) + n2

--reverse
-- This doesn't work for cases where there is a 0 in the middle of the
-- number, ex 9008 will return 89 instead of 8009
-- revN :: Integer -> Integer
-- revN 0 = 0
-- revN n = fastcat (mod n 10) (revN (div n 10))

-- Jon's fix for the above problem
revN :: Integer -> Integer
revN n = ff n 0
  where
    ff 0 es = es
    ff ns es =  ff (div ns 10) (es * 10 + mod ns 10)

--sum
sumN :: Integer -> Integer
sumN 0 = 0
sumN n = (mod n 10) + sumN (div n 10)

--product
prodN :: Integer -> Integer
prodN 0 = 1
prodN n = (mod n 10) * prodN (div n 10)

--general operation
type Binary = Integer -> Integer -> Integer

opN :: Binary -> Integer -> Integer -> Integer
opN bin base 0 = base
opN bin base n = bin (mod n 10) (opN bin base (div n 10))


