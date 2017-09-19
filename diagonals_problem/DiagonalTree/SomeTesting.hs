module SomeTesting where
import Prelude hiding (traverse)
import TreeTraversal
import Conditions
import ZipperTree
import Traversal

-- something isn't yet sitting right with
-- testing incrementing on i. verify what
-- should be done here.
test :: [Integer]
test = ff 0
  where
    ff i | triOrBetter (traverse i freeZip) = (getVal.traverse i) freeZip : ff (i+1)
         | otherwise = ff (i+1)

triNum :: Integer -> Integer
triNum n = div (n^2 + n) 2

triOrBetter :: Traversal Integer -> Bool
triOrBetter trav = (counts.getVal) trav >= (triNum 7 )
  where
    counts 0 = 0
    counts n | (mod n 10) == 0 = counts $ div n 10
             | otherwise = counts (div n 10) + 1

{-- Todo
  generalize functionality to work with any grid size.
--}