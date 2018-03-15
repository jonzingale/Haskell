module Graphs where
import Helpers

triangle = degreesToGraph [2,2,2]
simplex n = degreesToGraph.take (n+1) $ repeat n 
exampleGraph = degreesToGraph [5,4,3,4,1,1,1]

degreesToGraph :: Degrees -> Graph
degreesToGraph = vertsToGraph.degreesToVerts

data Vertex = V { name::String, degree::Int} deriving Eq
data Edge = E { source::Vertex, target::Vertex }
data Graph = G { edges::[Edge] } deriving Show
type Degrees = [Int]

vertices :: Graph -> [Vertex]
vertices = (map source).edges

instance Show Vertex where
  show (V a b) = a
instance Show Edge where
  show (E a b) = show a ++ "->" ++ show b

instance Ord Vertex where
  (<=) (V ss n) (V tt m) = n <= m
  (>=) (V ss n) (V tt m) = n >= m

vertsToGraph :: [Vertex] -> Graph
vertsToGraph verts = G $ hh [] verts
  where
    havel ((V ss n):as) = qsort $ snd_map (+ (-1)) (take n as) ++ drop n as
    toEdges ((V ss n):as) = [E (V ss n) vert | vert <- take n as]
    snd_map f xs =  [V a (f b)  | (V a b) <- xs]
    hh edgeAccum [] = edgeAccum
    hh edgeAccum verts =
      let sorted = qsort verts in
      hh (edgeAccum ++ toEdges sorted) (havel sorted)

degreesToVerts :: Degrees -> [Vertex]
degreesToVerts ds =  [V (show ss) d | (ss, d) <- zip [1..] ds]
