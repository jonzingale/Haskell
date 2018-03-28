module Solution where
import Data.List
{--
A riffle shuffle is executed as follows: a deck of cards is split into two
equal halves, with the top half taken in the left hand and the bottom half 
taken in the right hand. Next, the cards are interleaved exactly, with the top 
card in the right half inserted just after the top card in the left half, 
the 2nd card in the right half just after the 2nd card in the left half, 
etc. (Note that this process preserves the location of the top and
bottom card of the deck)

Let s(n) be the minimum number of consecutive riffle shuffles needed to 
restore a deck of size n to its original configuration, where n is a 
positive even number.

Amazingly, a standard deck of 52 cards will first return to its original 
configuration after only 8 perfect shuffles, so s(52)=8. It can be verified 
that a deck of 86 cards will also return to its original configuration after 
exactly 8 shuffles, and the sum of all values of n that satisfy s(n)=8 is 412.

Find the sum of all values of n that satisfy s(n)=60.
--}

{-- 
(((2 + 2^0) + 2^1) + 2^2) + ... + 2^60 `mod` (n-1) == 2
2 + (2^0 + 2^1 + ... 2^60) `mod` (n-1) == 2

10^7 => 23053423296
10^8 => 23053423296 + (234987393302) == 258040816598
10^9 => 258040816598 + () == 
--}

testable = f 1000000000 258040816598 -- 10^8
  where
    f 10000000000 acum = acum -- 10^9
    f n acum | gg' n && ff' n = f (n+2) (n+acum)
             | otherwise = f (n+2) acum

    ff' n = and [mod i (n-1) /= 1 | i <- [1073741824, 1048576, 32768, 4096]] -- [30,20,15,12]
    gg' n = mod 1152921504606846976 (n-1) == 1 -- 2^60

simpler = f 2 0
  where
    f 258 acum = acum
    f n acum | dd n && cc n = f (n+2) (n+acum)
             | otherwise = f (n+2) acum

    cc = \n-> mod 16 (n-1) /= 1
    dd = \n -> mod 256 (n-1) == 1

ff = \n-> and [mod (2^i) (n-1) /= 1 |i<-[30,20,15,12,10,6,5,4,3,2]]
gg = \n -> mod (2^60) (n-1) == 1

{--
It appears that if one just follows the second card in the deck, the
card will drift in powers of 2 to the right modulo the length of the
deck minus one. Relying on this observation, the following recursive
method calculates how long until the card returns to its initial place.
--}

-- old school methods.
euler622 = f 20000000 0
  where
    f 1000000000 acc = acc + 8750306960
    f n acc | riffleId (2*n) == 60 = f (n+1) (2*n+acc)
            | otherwise = f (n+1) acc

riffleId n = f 2 1 n
  where
    f 1 i n = i
    f p i n | i > 60 = 0 -- for pathological cases, which shouldn't exist.
            | otherwise = f (mod (p+2^i) (n-1)) (i+1) n

euler622' = f 30
  where
    f 10000 = []
    f n | riffleId (2*n) == 60 = (2*n):f (n+1)
        | otherwise = f (n+1)

-- Provables:
sixties = f 62
  where
    f n | riffleId' n == 60 = n : f (n+2)
        | otherwise = f (n+2)

riffleId' n = f (riffleOnce [1..n]) [1..n] 1
  where
    half as = div (length as) 2
    takeHalf as = [take (half as) as, drop (half as) as]
    riffleOnce = concat . transpose . takeHalf
    f j const k | j == const = k
                | otherwise = f (riffleOnce j) const (k+1) 

--length 56
fromTestable = [62,144,176,184,226,288,306,326,370,386,404,428,430,496,526,534,550,672,716,756,794,862,916,976,1002,1086,1156,1210,1282,1288,1322,1396,1436,1526,1576,1600,1656,1846,1892,1926,1964,2014,2016,2136,2146,2266,2276,2380,2476,2502,2584,2666,2746,2822,2926,3004]
fromSixties  = [62,144,176,184,226,288,306,326,370,386,404,428,430,496,526,534,550,672,716,756,794,862,916,976,1002,1086,1156,1210,1282,1288,1322,1396,1436,1526,1576,1600,1656,1846,1892,1926,1964,2014,2016,2136,2146,2266,2276,2380,2476,2502,2584,2666,2746,2822,2926,3004]