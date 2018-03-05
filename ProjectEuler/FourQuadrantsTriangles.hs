module FourQuadrantsTriangles where
import Data.List -- for partition
import Triangles


-- 4 Quadrants

-- calculate y and x intercepts for each line.
-- there must be a positive and negative intercept
-- for each.

type Triangle = [(Integer, Integer)]

coords :: Triangle -> [Integer]
coords [(a,b),(c,d),(e,f)] = [a,b,c,d,e,f]

onZero :: Triangle -> Bool
onZero = any (== 0).coords

verticalEdge :: Triangle -> Bool -- same Xs
verticalEdge [] = False
verticalEdge ((p,q):ts) | any (== p) $ map fst ts = True
                        | otherwise = verticalEdge ts 

{--
perhaps a do block which recursively performs each
function on a list, sums and passes the remaining list.
The first list in the partition are the True values.
--}

partsOnZero, partsOnVerts :: [Triangle] -> ([Triangle], [Triangle])
partsOnZero = partition onZero
partsOnVerts = partition verticalEdge


--all of these
-- crossesPositiveY :: Triangle -> Bool
-- crossesPositiveX :: Triangle -> Bool 
-- crossesNegativeY :: Triangle -> Bool 
-- crossesNegativeX :: Triangle -> Bool 