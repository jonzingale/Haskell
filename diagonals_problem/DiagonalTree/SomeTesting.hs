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
    ff i | triOrBetter (traverse i freeZip) =
            (getVal.traverse i) freeZip : ff (i+1)
         | otherwise = ff (i+1)

triNum :: Integer -> Integer
triNum n = div (n^2 + n) 2

triOrBetter :: Traversal Integer -> Bool
triOrBetter trav = (counts.getVal) trav >= (triNum (fromIntegral grid) + 0)

counts :: Integer -> Integer
counts 0 = 0
counts n | (mod n 10) == 0 = counts $ div n 10
         | otherwise = counts (div n 10) + 1

pp :: Integer -> IO()
pp n = putStr $ (show (n, counts n)) ++ "\n" ++ (unlines.ff.gg.show) n
  where
    ff [] = []
    ff str = take grid str : ff (drop grid str)
    gg [] = ""
    gg ('1':cs) = "\\" ++ gg cs
    gg ('0':cs) = '_' : gg cs
    gg ('2':cs) = '/' : gg cs

fff [] = []
fff str = take grid str : fff (drop grid str)

ggg [] = ""
ggg ('1':cs) = "\\" ++ ggg cs
ggg ('0':cs) = '_' : ggg cs
ggg ('2':cs) = '/' : ggg cs

doit = mapM_ pp test