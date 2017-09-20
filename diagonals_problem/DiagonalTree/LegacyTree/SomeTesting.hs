module SomeTesting where
import Prelude hiding (traverse)
import TreeTraversal
import Conditions
import ZipperTree
import Traversal

{--
Some how I am double (quad) counting on the Tree /|\
--}

test :: [Traversal Integer]
test = ff 0
  where
    ff i | and [len i, triguy i] = (traverse i) freeZip : ff (i+1)
         | otherwise = ff (i+1)
    nextzip j = traverse j freeZip
    triguy j = (triBool.nextzip) j -- only print tri
    len j = (grid^2) == (getHeight.nextzip) j -- long

triBool :: Traversal Integer -> Bool
triBool trav = (counts.getVal) trav == tri grid
  where
    tri n = div (n^2 + n) 2
    counts 0 = 0
    counts n | (mod n 10) == 0 = counts $ div n 10
             | otherwise = counts (div n 10) + 1

pp :: Traversal Integer -> IO()
pp trav = putStr $ tt trav
  where
    tt trav = show (getVal trav, getHeight trav) ++ 
      "\n" ++ (unlines.ff.gg.show) (getVal trav)
    ff [] = []
    ff str = let size = fromIntegral grid in
        take size str : ff (drop size str)
    gg [] = ""
    gg ('1':cs) = "\\" ++ gg cs
    gg ('0':cs) = '_' : gg cs
    gg ('2':cs) = '/' : gg cs

doit = mapM_ pp test
