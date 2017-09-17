module TreeTraversal where
import Prelude hiding (traverse)
import ExampleTree
import DiagonalTrees

exampleZip = (exampleTree, [], Zero)
freeZip = (freeTree, [], Zero)
theVerge = list2tree [2,1,1,2,0,2,1,0,0,1] (freeTree, [], Two)

test = focus $ list2tree [2,1,1,2,0,2,1,0,0,1] freeZip
test2 = focus $ list2tree [2,1,1] exampleZip

{--
I do need a Traversal data structure.
There needs to be a stack of instructions
which are unambiguous at each step of the
computation. As I traverse, there needs
to be a sense for where to proceed to next.
--}

traverse :: Zipper a -> Zipper a
traverse (Node pq l c r, bs, flag) |
  or [cond, flag == Two] = incrementFlag.goUp $ (Node pq l c r, bs, flag)
                                   | otherwise = (Node pq l c r, bs, flag)
  where
    cond = True

incrementFlag :: Zipper a -> Zipper a
incrementFlag (t, bs, Zero) = (t, bs, One)
incrementFlag (t, bs, One) = (t, bs, Two)
incrementFlag (t, bs, Two) = (t, bs, Full)
incrementFlag (t, bs, Full) = (t, bs, Full)


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