module ExampleTree where
import DiagonalTrees

{--
freeTree defaults to 1 in it's first node.
this ensures that all nodes along a path
contribute to the length of the accumulated
string.
--}

list2tree :: [Integer] -> Zipper a -> (a, Integer)
list2tree (a:as) z = list2tree as $ tern2tree a z 
list2tree [] z = (focus z, height z)

tern2tree :: Integer -> Zipper a -> Zipper a
tern2tree 1 z = z -: goLeft
tern2tree 0 z = z -: goCenter
tern2tree 2 z = z -: goRight

freeTree :: Tree Integer
freeTree = tree 1
  where
    tree n = Node n (tree (1+n*10)) (tree (n*10)) (tree (2+n*10))

exampleTree :: Tree Integer
exampleTree =
  Node 0
    (Node 01
      (Node 011
        (Node 0110 E E E)
        (Node 0111 E E E)
        (Node 0112 E E E)
      )
      (Node 010
        (Node 0101 E E E)
        (Node 0100 E E E)
        (Node 0102 E E E)
      )
      (Node 012
        (Node 0121 E E E)
        (Node 0120 E E E)
        (Node 0122 E E E)
      )
    )
    (Node 00
      (Node 001
        (Node 0011 E E E)
        (Node 0010 E E E)
        (Node 0012 E E E)
      )
      (Node 000
        (Node 0001 E E E)
        (Node 0000 E E E)
        (Node 0002 E E E)
      )
      (Node 002
        (Node 0021 E E E)
        (Node 0020 E E E)
        (Node 0022 E E E)
      )
    )
    (Node 02
      (Node 021
        (Node 0211 E E E)
        (Node 0210 E E E)
        (Node 0212 E E E)
      )
      (Node 020
        (Node 0201 E E E)
        (Node 0200 E E E)
        (Node 0202 E E E)
      )
      (Node 022
        (Node 0221 E E E)
        (Node 0220 E E E)
        (Node 0222 E E E)
      )
    )