module SomeTesting where
import Prelude hiding (traverse)
import TreeTraversal
import Conditions
import ZipperTree
import Traversal

test :: [Integer]
test = [(getVal.traverse i) freeZip | i<-[0..],
          triOrBetter (traverse i freeZip)]

triNum :: Integer -> Integer
triNum n = div (n^2 + n) 2

triOrBetter :: Traversal Integer -> Bool
triOrBetter trav = (counts.getVal) trav >= triNum 7
  where
    counts n | (mod n 10) == 0 = counts $ div n 10
             | otherwise = counts (div n 10) + 1

{-- Todo
  generalize functionality to work with any grid size.
--}