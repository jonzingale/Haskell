module TreeTraversal where
import Prelude hiding (traverse)
import ExampleTree
import DiagonalTrees

freeZip = (freeTree, [])
theVerge = list2tree [2,1,1,2,0,2,1,0,0,1] (freeTree, [])

test = focus $ list2tree [2,1,1,2,0,2,1,0,0,1] freeZip
-- test2 = focus $ blink theVerge
{--
I do need a Traversal data structure.
There needs to be a stack of instructions
which are unambiguous at each step of the
computation. As I traverse, there needs
to be a sense for where to proceed to next.
--}

-- traverse :: (Ord b, Num b) => Int -> Zipper (a, b) -> Zipper (a, b)
-- traverse 0 zs = zs
-- traverse n zs = traverse (n-1) (blink zs)

cond :: (Ord b, Num b) => Tree (a, b, c) -> Bool
cond (Node (p, height, flag) r s t) =  height >= 49

-- This needs very much work.
blink :: (Ord b, Num b) => Zipper (a, b, Flag) -> Zipper (a, b, Flag)
blink (tree, bs) | and [cond tree, getFlag (tree, bs) == Zero] =
                    incrFlag.goUp $ (tree, bs)
                 | otherwise = goRight(tree, bs)

incrFlag :: Zipper (a, b, Flag) -> Zipper (a, b, Flag) 
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