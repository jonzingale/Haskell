module MixedSpandrel where
import Math.NumberTheory.Primes.Factorisation
import System.Random
import Sortable
import Spandrel

{--
The idea: Assign to each block of shape type a 'key' prime, 2 say.
Define a map f :: Integer -> Shape, where f(n) is a shape whose key prime
divides n.

Distinct primes make a fiber over the integers whose projection map is related
by multiplication (except a square free inclusion). Assignments to blocks of a
given shape type are effectively given by sections of this projection.
--}

distinctPrimes :: Integer -> [Integer]
distinctPrimes = (map fst).factorise

shapeToInteger :: Int -> Shape -> Integer
shapeToInteger seed shape =
  case shape of
  Circle -> rep 2 seed
  Triangle -> rep 11 seed
  Square -> rep 13 seed
  where
    -- range is choosen small for readability
    rep n s = (* n).fst.randomR (1, 100) $ mkStdGen s

buildBlocks :: [Shape] -> [Pair Shape Integer]
buildBlocks shapes = [incl shape seed | (shape, seed) <- zip shapes randos]
  where
    randos = randoms (mkStdGen 42) :: [Int]
    incl shape s = rmap (shapeToInteger s) $ diag shape

compositeSort :: IO [Integer]
compositeSort = do -- builds from KeySortable class and shapes
  let shapes = take 300 $ cycle [Circle, Square, Triangle, Square, Circle]
  let blocks = buildBlocks shapes
  let sortedBlocks = sort blocks
  return $ map pr2 sortedBlocks
