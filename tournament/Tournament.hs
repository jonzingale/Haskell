module Tournament where
import System.Random
{-- 
This is a module for creating graphs for McMahon tournaments
using the Havel-Hakimi algorithm.

Todo: pass an argument so as not to have to set the seed.
Can this be simplified with a Monad? I am doing too much
threading.
--}
type Weighted = [(Degree, Vertex)]
type Graph = [(Int, Int)]
type Vertex = Int
type Degree = Int
type Seed = Int

--- Graph building
tournament :: Seed -> Int -> Graph
tournament s n | n < 8 || odd n = []
               | otherwise = hh s [] $ zip (take n $ repeat 3 ) $ take n [1..]
  where
    f ((a,b):as) = qsort $ fst_map (+ (-1)) (take a as) ++ drop a as
    edges ((a,b):as) = [(b, q) | (p,q) <- take a as]
    fst_map f xs =  [(f a, b)  | (a,b) <- xs]
    hh seed edge_list [] = edge_list
    hh seed edge_list pairs = let sorted = part_shuffle seed $ qsort $ pairs in
      hh seed (edge_list ++ edges sorted) $ f sorted

-- shuffling/sorting
part_shuffle :: Seed -> Weighted -> Weighted
part_shuffle s [] = []
part_shuffle s ((x,y):xs) = let cond = (== x).fst in
  (key_shuffle s $ (takeWhile cond ((x,y):xs))) ++
  (part_shuffle s $ dropWhile cond ((x,y):xs))

key_shuffle :: Ord a => Seed -> [a] -> [a]
key_shuffle seed xs = map snd $ qsort.zip (rands seed xs) $ xs
  where
    rands s as = take (length as) $ spitRandos s (2^(length as)) -- SEED

spitRandos :: Seed -> Int -> [Int]
spitRandos s n = randomRs (0,n) (mkStdGen s)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller
           where
             smaller = [s | s <- xs, s <= x]
             larger  = [l | l <- xs, l > x]