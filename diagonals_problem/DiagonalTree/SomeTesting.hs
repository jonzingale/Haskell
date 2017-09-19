module SomeTesting where
import Prelude hiding (traverse)
import TreeTraversal
import Conditions
import ZipperTree
import Traversal

-- rewrite this as corecursion so as
-- to not fill up memory.

test :: [Integer]
test = let trav j = (\t -> traverse t $ freeZip) j in 
  [getVal.trav $ i | i<-[0..],
    and [triOrBetter (trav i), (getHeight.trav) i == 49]]

triNum :: Integer -> Integer
triNum n = div (n^2 + n) 2

triOrBetter :: Traversal Integer -> Bool
triOrBetter trav = (counts.getVal) trav >= (triNum 7)
  where
    counts 0 = 0
    counts n | (mod n 10) == 0 = counts $ div n 10
             | otherwise = counts (div n 10) + 1

{-- Todo
  generalize functionality to work with any grid size.
--}

pp :: Integer -> IO()
pp n = putStr $ (show n) ++ "\n" ++ (unlines.ff.gg.show) n
  where
    ff [] = []
    ff str = take 7 str : ff (drop 7 str)
    gg [] = "\n"
    gg ('1':cs) = "\\" ++ gg cs
    gg ('0':cs) = '_' : gg cs
    gg ('2':cs) = '/' : gg cs

doit = mapM_ pp test
