module DataGraph where
import Data.Graph.Automorphism (isIsomorphic)
import MatrixMetrics
import Data.Graph
import Helpers

graph, biGraph :: Maybe Graph
graph = adjacencyG [5,4,4,3,2,2]
biGraph = biAdjacencyG [5,4,4,3,2,2]

biAdjacencyG :: [Int] -> Maybe Graph
biAdjacencyG list | havelhakimi list =
  let (Just gg) = adjacencyG list in
  let es g = foldr (++) [] $ map edges g in
  Just $ buildG (1, 6) $ es [gg, transposeG gg]
                  | otherwise = Nothing

adjacencyG :: [Int] -> Maybe Graph
adjacencyG list | havelhakimi list = let len = length list in
  Just $ buildG (1, len) $ nameEm $ f list [] len 1 list
                | otherwise = Nothing
  where
    keySort (l:ls) = snd.unzip.hhsort.zip ls
    hh (a:as) = map (subtract 1) (take a as) ++ drop a as
    zeros n = take n $ repeat 0
    ones  n = take n $ repeat 1

    buildR a ary n i ls = ary ++ [zeros i ++ keySort ls (ones a ++ zeros (n-a-1))]

    f [a] accum n i ls = buildR a accum n i ls
    f (a:as) accum n i ls = buildR a accum n i ls ++
                            f (hhsort.hh $ a:as) accum (n-1) (i+1) (hh (a:as))

    nameEm rows = [ (srcName, tarName)| (srcName, row) <- zip [1..] rows,
                                          (r, tarName) <- zip row [1..], r == 1]

-- hmm, naming (ie. Bounds) and direction matter. 
-- e2 = buildG (0,2) [(0,1),(0,2),(1,2)]
-- e1 = buildG (0,2) [(1,2),(1,0),(2,0)]
-- isIsomorphic e1 e2

graph2 = buildG (0, 5) [(0,1), (0,2), (0,3), (0,4), (0,5),
                (1,2), (1,3), (1,4), (2,3), (2,5)]

testIso = let (Just gg) = graph in isIsomorphic gg graph2 -- false by naming.
