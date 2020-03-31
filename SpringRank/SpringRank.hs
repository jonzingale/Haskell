module SpringRank where

import Numeric.LinearAlgebra.Sparse
import Data.Sparse.SpMatrix (SpMatrix)
import Data.Sparse.SpVector (SpVector, createv)
import Data.Sparse.Common (diagonalSM)

import Adjacency (mkAdjacency, nodes, tokenize)
import CsvParser (kirchoff)
import NumericalHelper

springrank = do
  k <- kirchoff
  let adj = mkAdjacency k
  let n = length.nodes.tokenize $ k
  let one = createv.take n $ repeat 1

  let k_in = adj #> one
  let k_out = transpose adj #> one

  let c = adj + transpose adj

  let d1 = diagonalSM $ k_in + k_out
  let d2 = k_out - k_in

  let b = one + d2
  let aa = eye n + d1 - c

  solved <- (aa <\> b)
  print $ map snd $ toListSV solved
