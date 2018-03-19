module DataGraph where
import Data.Graph
import Helpers hiding (hhAdjacency)

graph = hhAdjacency [5,4,3,3,2,2]
graph2 = buildG (1,6) [(1,2),(1,3),(1,4),(1,5),(1,6),(2,3),(2,5),(3,4),(3,6)]

hhAdjacency :: [Int] -> Graph
hhAdjacency list = buildG (1, length list) $ f list (length list) 1
  where
    array a n i = foldr (++) [] [zeros i, ones a, zeros (n-a-1)]
    zeros n = take n $ repeat 0
    ones  n = take n $ repeat 1

    f (a:[]) n i = w i $ array a n i
    f (a:as) n i = w i (array a n i) ++ f (g (a:as)) (n-1) (i+1)
    g (a:as) = hhsort $ map (subtract 1) (take a as) ++ drop a as

    w name [] = []
    w name (r:rs) | r == 1 = (name, length (r:rs)) : w name rs
                  | otherwise = w name rs
