module Adjacency where
import Data.Sparse.SpMatrix (SpMatrix, fromListSM)
import Data.List (elemIndex)
import Data.List.Unique (sortUniq)
import Data.Maybe (fromJust)

import Numeric.LinearAlgebra.Sparse (prd, prd0)
import CsvParser

type Rankings = [(Int, Double)]

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

detokenize :: Graph -> Rankings -> Rankings
detokenize edges ranks = zip (nodes edges) (map snd ranks)

toAdjacency :: Graph -> SpMatrix Double
toAdjacency graph = let num = length.nodes $ graph in
  fromListSM (num, num) [(s, t, v) | (Edge s t v) <- graph, v /= 0]
