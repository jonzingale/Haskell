module MixedSpandrel where
import Math.NumberTheory.Primes.Factorisation
import System.Random
import Sortable
import Spandrel

{--
The idea: Assign to each block of shape type a 'key' prime, 2 say. Define a map
f :: Integer -> Shape, where f(n) is a shape whose key prime is a prime factor
of n.

Distinct primes make a fiber over the integers whose projection map is related
by multiplication (except a square free inclusion). Assignments to blocks of a
given shape type are effectively given by sections of this projection.
--}

randomSection :: [Integer] -> [Integer]
randomSection ns = zipWith choice randos ns
  where
    distinctPrimes = (map fst).factorise
    randos = randoms (mkStdGen 42) :: [Int]
    choice seed n = 
      let dp = distinctPrimes n in
      let rand = seed `mod` length dp in
      dp!!rand

numberToShape :: Integer -> Shape
numberToShape x =
  case x of
  2 -> Circle
  3 -> Square
  5 -> Triangle

compositeSort :: IO [Color]
compositeSort = do -- builds from KeySortable class and shapes
  let blocks = [Circle, Square, Triangle, Square, Circle, Triangle, Square]
  let morph = map ((rmap f).diag) $ blocks
  let colors = map pr2 $ sort (morph :: [Pair Shape Color])
  return colors
  where
    -- How to make this map work?
    -- f Circle = select a random number divisible by 2
    f Circle = Red
    f Square = Yellow
    f Triangle = Blue
