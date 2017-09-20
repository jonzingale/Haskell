module ZipperTree where

data Tree a = E | Node a (Tree a) (Tree a) (Tree a) deriving (Show, Eq)
data Crumb a = LeftCrumb a (Tree a) (Tree a)   |
               CenterCrumb a (Tree a) (Tree a) |
               RightCrumb a (Tree a) (Tree a) deriving (Show, Eq)

type Zipper a = (Tree a, BreadCrumbs a)
type BreadCrumbs a = [Crumb a]

goLeft :: Zipper a -> Zipper a
goLeft (Node x l c r, bs) = (l, LeftCrumb x c r :bs)

goCenter :: Zipper a -> Zipper a
goCenter (Node x l c r, bs) = (c, CenterCrumb x l r :bs)

goRight :: Zipper a -> Zipper a
goRight (Node x l c r, bs) = (r, RightCrumb x l c :bs)

-- forget the data once exhausted: E
goUp :: Zipper a -> Zipper a
goUp (t, LeftCrumb x c r   :bs) = (Node x E c r, bs)
goUp (t, CenterCrumb x l r :bs) = (Node x l E r, bs)
goUp (t, RightCrumb x l c  :bs) = (Node x l c E, bs)
goUp (t, []) = (t, [])