module SpringRank where

import Numeric.LinearAlgebra.Sparse
import Data.Sparse.SpMatrix (SpMatrix, sparsifySM, nrows)
import Data.Sparse.SpVector (SpVector, createv, sparsifySV, onesSV)
import Data.Sparse.Common (diagonalSM)

import Adjacency (mkAdjacency, nodes, tokenize)
import CsvParser (kirchoff)
import NumericalHelper

-- ('1', 1.25)
-- ('11', 1.0694444444444442)
-- ('21', 1.0249999999999995)
-- ('3', 0.62500000000000011)
-- ('22', 0.22499999999999964)
-- ('12', 0.18055555555555525)
-- ('2', 0.0)

shiftRank :: SpVector Double -> SpVector Double
shiftRank v = let minV = constv (length v) (minimum v) in v - minV

springrank = do
  k <- kirchoff
  let adj = mkAdjacency k -- Matrix
  let n = nrows adj -- Int
  let one = onesSV n -- Vector

  let c = adj + transpose adj -- Matrix
  let k_out = transpose adj #> one -- Vector
  let k_in = adj #> one -- Vector

  let d1 = diagonalSM $ k_in + k_out -- Matrix
  let d2 = k_out - k_in -- Vector

  let a = sparsifySM $ eye n + d1 - c -- Matrix
  let b = sparsifySV $ one + d2 -- Vector

  solution <- a <\> b -- Vector
  print $ shiftRank solution
