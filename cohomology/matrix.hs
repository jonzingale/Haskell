module Matrix where
import qualified Numeric.Matrix as M
import qualified Data.Vector as V
import Data.Matrix

type ZMatrix = M.Matrix Integer
type RMatrix = M.Matrix Float

tetra :: ZMatrix
tetra = M.fromList [[0,1,1,1],[1,0,1,1],[1,1,0,1],[1,1,1,0]]

m1 :: ZMatrix
m1 = M.fromList [[1,2,3],[4,5,6],[7,8,8]]

m2 :: RMatrix
m2 = M.fromList [[1.0,2.0,3.0],[4.0,5.0,6.0],[7.0,8.0,8.0]]

rank2 = M.rank m2