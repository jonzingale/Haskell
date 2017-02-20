module HavelHakimi (havelhakimi, true_for, tournament) where
-- :set +s

type Edge = (Int, Int)
type Vertex = Int

{-- Tournaments:
A Tournament where each competitor plays seven distinct competitors
not including themselves exactly once is represented by a non-directed
graph where each vertex has degree 7.

From Havel-Hakimi we find that there exist tournaments for
all even numbers of competitors larger than 7.

note that `havelhakimi [2,2,2,1]` gives an example of an
even length sequence which is not graphic.
--}

true_for = [ i | i <- [1..], havelhakimi $ sevens i]

havelhakimi :: [Int] -> Bool
havelhakimi (a:[]) = a == 0
havelhakimi (a:as) = havelhakimi.qsort $
  map (+ (-1)) (take a as) ++ drop a as

{--
If havelhakimi n returns true, at each step add
the edges (v1,v2), (v1,v3), ..., (v1,v(d1+1)) .
Since it is true of all even length sequences
whose value is a constant 7, we just perform the
algorithm in tournament directly.
--}

-- 300 people about 5 seconds, 1050 edges.
tournament :: Int -> [Edge]
tournament n | n < 8 || odd n = []
             | otherwise = hh [] $ zip (sevens n) $ take n [1..]

hh :: [Edge] -> [(Vertex, Int)] -> [Edge]
hh edge_list [] = edge_list
hh edge_list pairs = let sorted = qsort pairs in
  hh (edge_list ++ edges sorted) (f sorted)
  where
    f ((a,b):as) = qsort $ fst_map (+ (-1)) (take a as) ++ drop a as
    edges ((a,b):as) = [(b,q) | (p,q) <- take a as]
    fst_map f xs =  [(f a, b) | (a, b)<- xs]

--- helpers
sevens n = take n $ repeat 7

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller
           where
             smaller = [s | s<-xs, s<=x]
             larger  = [l | l<-xs, l > x]
