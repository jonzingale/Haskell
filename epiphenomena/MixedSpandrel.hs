module MixedSpandrel where
import Math.NumberTheory.Primes.Factorisation (factorise)
import System.Random
import Sortable
import Shape

{--
Assign to each block of shape type a 'key' prime, 13 for instance.
Pair with each block of a given shape type a number divisible by the key prime.
Sorting the blocks their shape gives density sorted numbers.

Distinct primes make a fiber over the integers whose projection map is related
by multiplication (a square free inclusion). Assignments to blocks of a given
shape type are effectively sections of this projection.
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
buildBlocks shapes = zipWith incl shapes randos
  where
    incl shape s = second (shapeToInteger s) $ diag shape
    randos = randoms (mkStdGen 42) :: [Int]

compositeSort :: IO [Integer]
compositeSort = do
  let shapes = take 300 shapeGen
  let blocks = buildBlocks shapes
  let sortedBlocks = sort blocks
  return $ map pr2 sortedBlocks

-- compositeSort' included to make result easier to inspect.
compositeSort' :: IO [[Integer]]
compositeSort' = do
  ls <- compositeSort
  let fs = map (f.distinctPrimes) ls
  return fs
  where
    distinctPrimes = (map fst).factorise
    f [] = []
    f (x:xs) =
      case x of
        2 -> 2 : f xs
        11 -> 11 : f xs
        13 -> 13 : f xs
        _ -> f xs
