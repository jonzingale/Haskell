module ExampleTree where
import DiagonalTrees

{--
freeTree defaults to 1 in it's first node.
this ensures that all nodes along a path
contribute to the length of the accumulated
string.
--}
data Flag = Zero | One | Two | Full deriving (Show, Eq)

type TreeZip = (Tree (Integer, Int, Flag), BreadCrumbs (Integer, Int, Flag))

freeTree :: Tree (Integer, Int, Flag)
freeTree = tree (1, 1, One)
  where
    tree (n, h, flag) =
      Node (n, h, flag) (tree (1+n*10, h+1, One))
                        (tree (0+n*10, h+1, Zero))
                        (tree (2+n*10, h+1, Two))


getFlag :: Zipper (a, b, c) -> c
getFlag (Node (n, h, flag) l c r, bs) = flag

setFlag :: Flag -> Zipper (a, b, Flag) -> Zipper (a, b, Flag)
setFlag flag (Node (n, h, f) l c r, bs) = (Node (n, h, flag) l c r, bs)