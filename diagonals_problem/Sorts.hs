module Sorts where

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (n:ns) = qsort(smaller n ns) ++ [n] ++ qsort(larger n ns)
  where
    smaller x xs = [ j | j<-xs, j <= x]
    larger x xs  = [ j | j<-xs, j > x]

sort_lasts :: Ord a => [(b, a)] -> [(b, a)]
sort_lasts [] = []
sort_lasts (n:ns) = sort_lasts(smaller n ns) ++ [n] ++ sort_lasts(larger n ns)
  where
    smaller x xs = [ (i, j) | (i, j)<-xs, j <= snd x]
    larger x xs  = [ (i, j) | (i, j)<-xs, j > snd x]

uniq :: Ord a => [a] -> [a]
uniq [] = []
uniq (n:ns) = uniq(smaller n ns) ++ [n] ++ uniq(larger n ns)
  where
    smaller x xs = [ j | j<-xs, j < x]
    larger x xs  = [ j | j<-xs, j > x]