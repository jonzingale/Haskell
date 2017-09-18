module TreeTraversal where
import Prelude hiding (traverse)
import ZipperTree
import Traversal

freeZip = (freeTree, [])

-- 49 log 49 ~ 191
traverse :: Int -> Traversal Integer -> Traversal Integer
traverse 0 zs = zs
traverse n zs = traverse (n-1) $ blink zs

cond :: Traversal Integer -> Bool
cond trav = or [cond1 trav, cond2 trav]

cond1 :: Traversal a -> Bool -- height condition
cond1 trav = getHeight trav >= 49

cond2 :: Traversal Integer -> Bool -- adjacency condition
cond2 trav | getHeight trav <= 7 = False
           | otherwise =
             let (n, a) = divMod (getVal trav) 10 in
             let b = mod n 10 in
             a + b == 3

blink :: Traversal Integer -> Traversal Integer
blink trav | cond trav = incrFlag.goUp $ trav
           | otherwise = step trav
  where
    step ts | getFlag ts == One = goLeft ts
            | getFlag ts == Zero = goCenter ts
            | getFlag ts == Two = goRight ts
            | getFlag ts == Full = incrFlag.goUp $ ts

incrFlag :: Traversal a -> Traversal a 
incrFlag zs | getFlag zs == One = setFlag Zero zs
            | getFlag zs == Zero = setFlag Two zs
            | getFlag zs == Two = setFlag Full zs
            | otherwise = zs

