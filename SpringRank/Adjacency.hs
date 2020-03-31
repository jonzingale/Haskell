module Adjacency where
import Data.Sparse.SpMatrix
import Data.List (elemIndex)
import Data.List.Unique (sortUniq)
import Data.Maybe (fromJust)

import CsvParser

mkAdjacency :: Graph -> SpMatrix Double
mkAdjacency = toAdjacency.tokenize.cleanMultiEdges

nodes :: Graph -> [Int]
nodes g = sortUniq $ map source g ++ map target g

cleanMultiEdges :: Graph -> Graph
cleanMultiEdges [] = []
cleanMultiEdges (e:es) =
  let same = filter (== e) (e:es) in
  let diff = filter (/= e) es in
  (foldr mappend mempty same) : cleanMultiEdges diff

tokenize :: Graph -> Graph
tokenize graph = let totals = nodes graph in
  [ Edge (justIndex s totals) (justIndex t totals) v | Edge s t v <- graph ]
  where
    justIndex s = fromJust.elemIndex s

toAdjacency :: Graph -> SpMatrix Double
toAdjacency graph =
  let num = length.nodes $ graph in
  let triples = [(s, t, v) | (Edge s t v) <- graph] in
  fromListSM (num, num) triples
