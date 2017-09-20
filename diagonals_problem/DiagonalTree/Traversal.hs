module Traversal where
import ZipperTree

{--
freeTree defaults to 1 in it's first node.
this ensures that all nodes along a path
contribute to the length of the accumulated
string.
--}

data Flag = Zero | One | Two | Full deriving (Show, Eq)
data Print = Null | Print deriving (Show, Eq)
--Perhaps I want a print flag?

type Traversal a = Zipper (a, Integer, Flag, Print)

freeTree :: Tree (Integer, Integer, Flag, Print)
freeTree = tree (1, 1, One, Null)
  where
    tree (n, h, flag, print) = Node (n, h, flag, print)
                        (tree (1+n*10, h+1, One, Null))
                        (tree (  n*10, h+1, One, Null))
                        (tree (2+n*10, h+1, One, Null))

getFlag :: Traversal a -> Flag
getFlag (Node (n, h, flag, print) l c r, bs) = flag

getHeight :: Traversal a -> Integer
getHeight (Node (n, height, flag, print) l c r, bs) = height

getVal :: Traversal a -> a
getVal (Node (n, height, flag, print) l c r, bs) = n 

getFocus :: Traversal a -> (a, Integer, Flag, Print)
getFocus (Node a l c r, bs) = a

getPrint :: Traversal a -> Print
getPrint (Node (n, h, f, print) l c r, bs)  = print

setFlag :: Flag -> Traversal a -> Traversal a
setFlag flag (Node (n, h, f, p) l c r, bs) = (Node (n, h, flag, p) l c r, bs)

setPrint :: Print -> Traversal a -> Traversal a
setPrint print (Node (n, h, f, p) l c r, bs) = (Node (n, h, f, print) l c r, bs)