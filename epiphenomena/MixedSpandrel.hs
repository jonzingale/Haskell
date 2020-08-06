module MixedSpandrel where
import Math.NumberTheory.Primes.Factorisation
import Data.Bifunctor (second)
import System.Random
import Sortable
import Shape

{--
The idea: Assign to each block of shape type a 'key' prime, 2 say.
Define a map f :: Integer -> Shape, where f(n) is a shape whose key prime
divides n.

Distinct primes make a fiber over the integers whose projection map is related
by multiplication (except a square free inclusion). Assignments to blocks of a
given shape type are effectively given by sections of this projection.
--}

shapeToInteger :: Int -> Shape -> Integer
shapeToInteger seed shape =
  case shape of
  Circle -> rep 2 seed
  Triangle -> rep 11 seed
  Square -> rep 13 seed
  where
    -- range is restricted for readability
    rep n s = (* n).fst.randomR (1, 100) $ mkStdGen s

 -- builds from KeySortable class and shapes
buildBlocks :: [Shape] -> [Pair Shape Integer]
buildBlocks shapes = [incl shape seed | (shape, seed) <- zip shapes randos]
  where
    randos = randoms (mkStdGen 42) :: [Int]
    incl shape s = second (shapeToInteger s) $ diag shape

compositeSort :: IO [Integer]
compositeSort = do
  let shapes = take 300 $ cycle [Circle, Square, Triangle, Triangle, Circle]
  let blocks = buildBlocks shapes
  let sortedBlocks = sort blocks
  return $ map pr2 sortedBlocks

-- filteredSort makes it visually easier to see that the sort works
filteredSort :: IO [[Integer]]
filteredSort = do
  ls <- compositeSort
  let fs = map (f.distinctPrimes) ls
  return fs
  where
    distinctPrimes = (map fst).factorise
    f [] = []
    f (x:xs) | x == 2 = 2 : f xs
             | x == 11 = 11 : f xs
             | x == 13 = 13 : f xs
             | otherwise = f xs
