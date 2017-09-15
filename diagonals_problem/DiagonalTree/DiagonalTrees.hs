module DiagonalTrees where
{--
* Work out a search algorithm, tree traversal.
--}

data Tree a = E | Node a (Tree a) (Tree a) (Tree a) deriving (Show, Eq)
data Crumb a = LeftCrumb a (Tree a) (Tree a)   |
               CenterCrumb a (Tree a) (Tree a) |
               RightCrumb a (Tree a) (Tree a) deriving (Show, Eq)

type Height = Integer
type BreadCrumbs a = [Crumb a]
type Zipper a = (Tree a, BreadCrumbs a, Height)

goLeft :: Zipper a -> Zipper a
goLeft (Node x l c r, bs, h) = (l, LeftCrumb x c r :bs, h+1)

goCenter :: Zipper a -> Zipper a
goCenter (Node x l c r, bs, h) = (c, CenterCrumb x l r :bs, h+1)

goRight :: Zipper a -> Zipper a
goRight (Node x l c r, bs, h) = (r, RightCrumb x l c :bs, h+1)

goUp :: Zipper a -> Zipper a
goUp (t, LeftCrumb x c r:bs, h) = (Node x t c r, bs, h-1)
goUp (t, CenterCrumb x l r:bs, h) = (Node x l t r, bs, h-1)
goUp (t, RightCrumb x l c :bs, h) = (Node x l c t, bs, h-1)
goUp (t, [], h) = (t, [], h)

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l c r, bs, h) = (Node (f x) l c r, bs, h)
modify f (E, bs, h) = (E, bs, h)

topMost :: Zipper a -> Zipper a
topMost (t, [], h) = (t, [], h)
topMost z = topMost (goUp z)

(-:) :: a -> (a -> b) -> b
(-:) x f = f x

focus :: Zipper a -> a
focus (Node a r s t, bs, h) = a

height :: Zipper a -> Integer
height (t, bs, h) = h
