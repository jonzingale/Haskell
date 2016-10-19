module Matrix where
import qualified Numeric.Matrix as M
import qualified Data.Vector as V
import Data.Matrix

type MMatrix = M.Matrix Float

m2 :: MMatrix
m2 = M.fromList [[1.0,2.0,3.0],[4.0,5.0,6.0],[7.0,8.0,8.0]]

rank2 = M.rank m2