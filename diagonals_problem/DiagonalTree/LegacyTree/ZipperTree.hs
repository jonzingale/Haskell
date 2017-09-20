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

goUp :: Zipper a -> Zipper a
goUp (t, LeftCrumb x c r:bs) = (Node x t c r, bs)
goUp (t, CenterCrumb x l r:bs) = (Node x l t r, bs)
goUp (t, RightCrumb x l c :bs) = (Node x l c t, bs)
goUp (t, []) = (t, [])

--- Below here mostly for testing.
(-:) :: a -> (a -> b) -> b
(-:) x f = f x

list2tree :: [Integer] -> Zipper a -> Zipper a
list2tree (a:as) z = list2tree as $ tern2tree a z 
list2tree [] z = z

tern2tree :: Integer -> Zipper a -> Zipper a
tern2tree 1 z = z -: goLeft
tern2tree 0 z = z -: goCenter
tern2tree 2 z = z -: goRight

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l c r, bs) = (Node (f x) l c r, bs)
modify f (E, bs) = (E, bs)

topMost :: Zipper a -> Zipper a
topMost (t, []) = (t, [])
topMost z = topMost (goUp z)

focus :: Zipper a -> a
focus (Node a r s t, bs) = a