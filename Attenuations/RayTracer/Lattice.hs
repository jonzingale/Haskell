
-- https://hackage.haskell.org/package/array-0.5.2.0/docs/Data-Array.html
module RayTracer.Lattice where
import RayTracer.Rhythm
import System.Random
import System.IO

-- import qualified Data.Array.Accelerate as A -- GPU
-- import qualified Data.Array.Repa as R -- wrapper for fast
import qualified Data.ByteString.Lazy as L
import qualified Data.Array.Unboxed as U -- strict fast Arrays
import qualified Data.Array.IArray as I -- fast lookups on Unboxed (I.!)

import qualified Data.Array as A -- mostly for comparison
import Data.List
{--
TODO:
Make a 1000 cubic-cell mesh and calculate the path sums through
the mesh with a function at every point. The value of the function
gives the amount of attenuation through the mesh.

Further, the length of the ray through each given
cell is needed to compute the value at the given cell.

10^7 => (12.37 secs, 24,200,154,144 bytes)
10^8 => (117.79 secs, 253,200,261,744 bytes)

10^9 cells eventually. ~20 mins
--}
-- 10^7 like a champ, 10^8 gets challenging.
-- rArray takes seed values to produce random arrays.

bigArray :: U.UArray Int Double
bigArray = U.listArray bounds $ randomRs (0, 10**3::Double).mkStdGen $ 42
  where bounds = (0::Int, 10^3-1)

exBigA = (I.!) bigArray 34345

-- saveArr = do
    -- outh <- openFile "./test.txt" WriteMode
    -- hPrint outh bigArray
    -- hClose outh
-- (435.28 secs, 1,273,407,019,768 bytes) 10^8

saveArr = do
  writeFile "./Tests/testArray.csv" $ unlines.map show $ U.elems bigArray
  -- L.writeFile ".Tests/testArray.csv" $ ((U.elems)) bigArray


displayChars = do
  content <- L.readFile "./Tests/testArray.csv"
  return (content)

-- import qualified Data.ByteString as B
-- main = B.readFile "/usr/share/dict/words" >>= B.putStr . last . B.lines