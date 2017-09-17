module TreeTraversal where
import Prelude hiding (traverse)
import ExampleTree
import DiagonalTrees

freeZip = (freeTree, [])

-- test = focus $ list2tree [2,1,1,2,0,2,1,0,0,1,2] freeZip
-- test2 = focus $ traverse 50 freeZip

-- 49 log 49 ~ 191
traverse :: Int -> Traversal Integer -> Traversal Integer
traverse 0 zs = zs
traverse n zs = traverse (n-1) $ blink zs

cond :: Traversal Integer -> Bool
cond trav = or [cond1 trav, cond2 trav]

cond1 :: Traversal a -> Bool -- height condition
cond1 trav = getHeight trav >= 49

cond2 :: Traversal Integer -> Bool -- adjacency condition
cond2 trav = let (n, a) = divMod (getVal trav) 10 in
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


{--
* Define a tree traversal, starting from the
left and working its way up and to the right.
* Write bounding functions:
  - only 49 deep
  - adjacency rules
  - CA like rules
* Return valid strings.

Where to start:
Extend the data structure of the Zipper to
include height and index data. perhaps height
can be embedded directly into the Node information.
--}

type N = Integer

upperNeigh :: N -> (N,N,N) -> Bool
upperNeigh 0 _ = True
upperNeigh 1 ns = any (== ns) $ both ++ [(0,0,2), (0,1,0), (0,1,1), (2,0,2)]
upperNeigh 2 ns = any (== ns) $ both ++ [(0,2,0), (1,0,0), (1,0,1), (2,2,0)]
both = [(0,0,0), (0,0,1), (2,0,0), (2,0,1)]

adjacentNeigh :: N -> N -> Bool
adjacentNeigh a b = a + b /= 3

lengthSeven :: N -> Bool
lengthSeven n = f n 0
  where
    f 0 i = i == 7
    f n i = f (div n 10) (i+1)

lengthFortyNine :: N -> Bool
lengthFortyNine n = f n 0
  where
    f 0 i = i == 49
    f n i = f (div n 10) (i+1)