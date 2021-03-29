module Qsort where

qsort :: Ord a => [a] -> [a]
qsort (x:xs) = (qsort.smallerEq x) xs ++ [x] ++ (qsort.larger x) xs
  where
    smallerEq x xs = [ n | n <- xs, n <= x ]
    larger x xs = [ n | n <- xs, n > x ]
qsort [] = []
