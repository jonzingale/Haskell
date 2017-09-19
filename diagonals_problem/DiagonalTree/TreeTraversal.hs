module TreeTraversal where
import Prelude hiding (traverse)
import Conditions
import ZipperTree
import Traversal

freeZip = (freeTree, [])

traverse :: Int -> Traversal Integer -> Traversal Integer
traverse 0 zs = zs
traverse n zs = traverse (n-1) $ blink zs

cond :: Traversal Integer -> Bool
cond trav = or [cond1 trav, cond2 trav, cond3 trav]

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

