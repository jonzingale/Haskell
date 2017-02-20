module HavelHakimi where

havelhakimi :: [Int] -> Bool
havelhakimi (a:[]) | a == 0 = True
                   | otherwise = False
havelhakimi (a:as) = havelhakimi $ qsort $
  map (+ (-1)) (take a as) ++ drop a as

--- helpers
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller
           where
             smaller = [s | s<-xs, s<=x]
             larger  = [l | l<-xs, l > x]

{-- Tournaments:
A Tournament where each competitor plays
seven distinct competitors besides themselves
exactly once is represented by a non-directed
graph where each vertex has degree 7.

`havelhakimi [2,2,2,1]` gives an example of an
even length sequence which is not graphic.

From Havel-Hakimi we find that there exist tournaments for
all even numbers of competitors larger than 7.
--}

true_fors = [ i | i <- [1..], havelhakimi $ sevens i]
  where
    sevens 0 = []
    sevens n = 7 : sevens (n-1)
    
