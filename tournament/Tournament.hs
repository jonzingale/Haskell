module Tournament where
import System.Random

type Weighted = [(Degree, Vertex)]
type Graph = [(Int, Int)]
type Vertex = Int
type Degree = Int
{-- 
This is a module for creating graphs for McMahon tournaments
using the Havel-Hakimi algorithm.

Todo: pass an argument so as not to have to set the seed.
--}

--- Graph building
tournament :: Int -> Graph
tournament n | n < 8 || odd n = []
             | otherwise = hh [] $ zip (take n $ repeat 3 ) $ take n [1..]
  where
    f ((a,b):as) = qsort $ fst_map (+ (-1)) (take a as) ++ drop a as
    edges ((a,b):as) = [(b, q) | (p,q) <- take a as]
    fst_map f xs =  [(f a, b)  | (a,b) <- xs]
    hh edge_list [] = edge_list
    hh edge_list pairs = let sorted = part_shuffle.qsort $ pairs in
      hh (edge_list ++ edges sorted) $ f sorted

-- shuffling/sorting
mkBlanket = mkStdGen

part_shuffle :: Weighted -> Weighted
part_shuffle [] = []
part_shuffle ((x,y):xs) = let cond = (== x).fst in
  (key_shuffle.takeWhile cond) ((x,y):xs) ++
  (part_shuffle.dropWhile cond) ((x,y):xs)

key_shuffle :: Ord a => [a] -> [a]
key_shuffle xs = map snd $ qsort.zip (rands xs) $ xs
  where
    rands as = take (length as) $ spitRandos 17 (2^(length as)) -- SEED

spitRandos :: Int -> Int -> [Int]
spitRandos s n = randomRs (0,n) (mkBlanket s)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller
           where
             smaller = [s | s <- xs, s <= x]
             larger  = [l | l <- xs, l > x]