module FourQuadrantsTriangles where
import Data.List -- for partition
import Triangles

{--
4 Quadrants

II,  I
III, IV

calculate y and x intercepts for each line.
there must be a positive and negative intercept
for each.
--}
type Triangle = [(Integer, Integer)]

data Quadrant = I | II | III | IV deriving (Eq, Show)

toQuad :: Triangle -> [Quadrant]
toQuad [] = []
toQuad (pt:pts) = f pt : toQuad pts
  where
    f (x,y) | and [x >= 0, y >= 0] = I
    f (x,y) | and [x <= 0, y >= 0] = II
    f (x,y) | and [x <= 0, y <= 0] = III
    f (x,y) | otherwise = IV

coords :: Triangle -> [Integer]
coords [(a,b),(c,d),(e,f)] = [a,b,c,d,e,f]

onZero :: Triangle -> Bool
onZero = any (== 0).coords

verticalEdge :: Triangle -> Bool -- same Xs
verticalEdge [] = False
verticalEdge ((p,q):ts) | any (== p) $ map fst ts = True
                        | otherwise = verticalEdge ts 

sameQuadrant :: Triangle -> Bool
sameQuadrant pts = f.toQuad $ pts
  where
    f (pt:pts) = all (== pt) pts
{--
perhaps a do block which recursively performs each
function on a list, sums and passes the remaining list.
The first list in the partition are the True values.

1) separately check verticalEdges first as they will not have slopes.
2) include any with a zero endpoint.
3) remove if every edge is in the same quadrant, because it doesn't contain 0.
--}

partsOnZero, partsOnVerts :: [Triangle] -> ([Triangle], [Triangle])
partsOnVerts = partition verticalEdge
partsOnZero = partition onZero
partsSameQuad = partition sameQuadrant


quadI :: Triangle -> Bool -- endpoint ++ or II->IV line passes zero or higher.
quadI triangle = any (== I) $ toQuad triangle
--all of these
-- crossesPositiveY :: Triangle -> Bool
-- crossesPositiveX :: Triangle -> Bool 
-- crossesNegativeY :: Triangle -> Bool 
-- crossesNegativeX :: Triangle -> Bool 