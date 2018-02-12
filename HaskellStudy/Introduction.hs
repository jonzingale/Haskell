module Introduction where

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