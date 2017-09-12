module DiagonalTrees where

{--
* Free generate a diagonals tree.
* Work out a search algorithm, tree traversal.
--}

(-:) :: a -> (a -> b) -> b
(-:) x f = f x

data Tree a = E | Node a (Tree a) (Tree a) (Tree a) deriving (Show, Eq)
data Crumb a = LeftCrumb a (Tree a) (Tree a)   |
               CenterCrumb a (Tree a) (Tree a) |
               RightCrumb a (Tree a) (Tree a) deriving (Show, Eq)

type BreadCrumbs a = [Crumb a]
type Zipper a = (Tree a, BreadCrumbs a)

goLeft :: Zipper a -> Zipper a
goLeft (Node x l c r, bs) = (l, LeftCrumb x c r :bs)

goCenter :: Zipper a -> Zipper a
goCenter (Node x l c r, bs) = (c, CenterCrumb x l r :bs)

goRight :: Zipper a -> Zipper a
goRight (Node x l c r, bs) = (r, RightCrumb x l c :bs)

goUp :: Zipper a -> Zipper a
goUp (t, LeftCrumb x c r:bs) = (Node x t c r, bs)
goUp (t, CenterCrumb x l r:bs) = (Node x l t r, bs)
goUp (t, RightCrumb x l c :bs) = (Node x l c t, bs)
goUp (t, []) = (t, [])

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l c r, bs) = (Node (f x) l c r, bs)
modify f (E, bs) = (E, bs)

topMost :: Zipper a -> Zipper a
topMost (t, []) = (t, [])
topMost z = topMost (goUp z)

anIdentity = (freeTree, []) -: goLeft -: goRight  -: topMost ==  (freeTree, [])

freeTree :: Tree Int
freeTree =
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