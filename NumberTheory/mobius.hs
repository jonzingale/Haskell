module Mobius where
import Data.Graph
import Data.POSet

{--
Graph will not work, needs a Partially Ordered Set
module to make this work even at all. Perhaps, I
could extend Data.Graph to include a top and bottom?

buildG :: Bounds -> [Edge] -> Graph
Bounds = (Vertex, Vertex)
Edge = (Vertex, Vertex)
Vertex = Int
--}


p1 :: Graph
p1 = buildG (0,4) [(0,1),(0,2),(0,3),(1,4),(2,4),(3,4)]

p2 :: Graph
p2 = buildG (0,4) [(0,1),(0,2),(2,4),(1,3),(3,4)]

-- Needs the graph context.
-- otherwise is computing over arbitrary numbers.
mobius :: Vertex -> Vertex -> Int
mobius x y | x == y = 1
           | otherwise = negate $ foldr (+) 0 [mobius x z | z<-[x..y-1]]

-- this needs to be over vertices
-- instance Ord Graph where
  -- (<)

-- isLess :: Graph -> Vertex -> Vertex -> Bool
-- isLess g x y | x 

-- getLess :: Graph -> Vertex -> [Vertex]
-- getLess g v = f (edges g) v
--   where
--     f es x | 