module SpringRank.SpringRank where

import Numeric.LinearAlgebra.Sparse
import Data.Sparse.SpMatrix (sparsifySM, nrows)
import Data.Sparse.SpVector (SpVector, sparsifySV, onesSV)
import Data.Sparse.Common (diagonalSM)

-- import CsvParser (Graph, getGraph)
import SpringRank.GoParser (genGraph)
import SpringRank.NumericalHelper
import SpringRank.Adjacency

example :: IO Rankings
example = springRank "data/kirchoff.dat"

springRank file = do
  -- graph <- getGraph file
  graph <- genGraph file
  let adj = mkAdjacency graph -- Matrix
  let n = nrows adj -- Int
  let one = onesSV n -- Vector

  let c = adj + transpose adj -- Matrix
  let k_in = transpose adj #> one -- Vector
  let k_out = adj #> one -- Vector

  let d1 = diagonalSM $ k_in + k_out -- Matrix
  let d2 = k_out - k_in -- Vector

  let a = sparsifySM $ eye n + d1 - c -- Matrix
  let b = sparsifySV $ one + d2 -- Vector

  solution <- a <\> b -- Vector
  let ranks = shiftRank solution
  return $ tokenize graph (toListSV ranks)

shiftRank :: SpVector Double -> SpVector Double
shiftRank v = let minV = constv (length v) (minimum v) in v - minV
