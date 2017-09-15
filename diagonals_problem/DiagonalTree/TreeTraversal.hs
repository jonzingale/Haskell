module TreeTraversal where
import ExampleTree
import DiagonalTrees

exampleZip = (exampleTree, [], 0)
freeZip = (freeTree, [], 0)

test = list2tree [2,1,1,2,0,2,1,0,0,1] freeZip
test2 = list2tree [2,1,1]  exampleZip

type N = Integer

{--
* Define a tree traversal, starting from the
left and working its way up and to the right.
* Write bounding functions:
  - only 49 deep
  - adjacency rules
  - CA like rules
* Return valid strings.
--}

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