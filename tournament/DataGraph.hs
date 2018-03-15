module DataGraph where
import Data.Graph

-- [(7,6),(5,3),(3,4),(3,2),(4,2),(4,1),(2,1)]
graph = buildG (0, 7) [(7,6),(5,3),(3,4),(3,2),(4,2),(4,1),(2,1)]