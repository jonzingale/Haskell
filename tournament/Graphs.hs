module Graphs where
import Helpers

triangle = degreesToGraph [2,2,2]
simplex n = degreesToGraph.take (n+1) $ repeat n 
exampleBadGraph = degreesToGraph [5,4,3,4,1,1,1] -- Bad
exampleGraph = degreesToGraph [5,4,3,4,2,2]
testVertices = map degree $ vertices exampleGraph

degreesToGraph :: Degrees -> Graph
degreesToGraph degs | havelhakimi degs = vertsToGraph.degreesToVerts $ degs
                    | otherwise = BadGraph

data Vertex = V { name::String, degree::Int} deriving Eq
data Edge = E { source::Vertex, target::Vertex } deriving Eq
data Graph = G { edges::[Edge] } | BadGraph deriving (Eq, Show)
type Degrees = [Int]

vertices :: Graph -> [Vertex]
vertices (G es) = let totalV = [target, source] <*> es in f totalV []
  where
    f [] accum = accum
    f ((V n d):vs) accum | any (\v -> name v == n) accum = f vs accum
                         | otherwise = f vs ((V n d):accum)

instance Show Vertex where
  show (V name deg) = name ++ " " ++ show deg
instance Show Edge where
  show (E a b) = (name a) ++ "->" ++ (name b)

instance Ord Vertex where
  (<=) (V ss n) (V tt m) = n <= m
  (>=) (V ss n) (V tt m) = n >= m

vertsToGraph :: [Vertex] -> Graph
vertsToGraph verts = rebuildDegs.G $ hh verts []
  where
    havel ((V ss n):as) = hhsort $ snd_map (+ (-1)) (take n as) ++ drop n as
    toEdges ((V ss n):as) = [E (V ss n) vert | vert <- take n as]
    snd_map f xs = [V a (f b) | (V a b) <- xs]

    hh [] edgeAccum = edgeAccum
    hh verts edgeAccum =
      let sorted = hhsort verts in
      hh (havel sorted) (edgeAccum ++ toEdges sorted)

rebuildDegs :: Graph -> Graph
rebuildDegs (G es) = G $ map (buildE es) es
  where
    buildV (V n d) tars = V n $ (length.filter (== n)) tars
    buildE es (E v1 v2) =
      let totalV = [name.target, name.source] <*> es in
      E (buildV v1 totalV) (buildV v2 totalV)


degreesToVerts :: Degrees -> [Vertex]
degreesToVerts ds =  [V (show ss) d | (ss, d) <- zip [1..] ds]