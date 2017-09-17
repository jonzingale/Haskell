module DiagonalTrees where
-- Very general Tree and Zipper info here.

{--
* Work out a search algorithm, tree traversal.
--}
data Flag = Zero | One | Two | Full deriving (Show, Eq)
data Tree a = E | Node a (Tree a) (Tree a) (Tree a) deriving (Show, Eq)
data Crumb a = LeftCrumb a (Tree a) (Tree a)   |
               CenterCrumb a (Tree a) (Tree a) |
               RightCrumb a (Tree a) (Tree a) deriving (Show, Eq)

type BreadCrumbs a = [Crumb a]
type Zipper a = (Tree a, BreadCrumbs a, Flag)

goLeft :: Zipper a -> Zipper a
goLeft (Node x l c r, bs, flag) = (l, LeftCrumb x c r :bs, flag)

goCenter :: Zipper a -> Zipper a
goCenter (Node x l c r, bs, flag) = (c, CenterCrumb x l r :bs, flag)

goRight :: Zipper a -> Zipper a
goRight (Node x l c r, bs, flag) = (r, RightCrumb x l c :bs, flag)

goUp :: Zipper a -> Zipper a
goUp (t, LeftCrumb x c r:bs, flag) = (Node x t c r, bs, flag)
goUp (t, CenterCrumb x l r:bs, flag) = (Node x l t r, bs, flag)
goUp (t, RightCrumb x l c :bs, flag) = (Node x l c t, bs, flag)
goUp (t, [], flag) = (t, [], flag)

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l c r, bs, flag) = (Node (f x) l c r, bs, flag)
modify f (E, bs, flag) = (E, bs, flag)

topMost :: Zipper a -> Zipper a
topMost (t, [], flag) = (t, [], flag)
topMost z = topMost (goUp z)

(-:) :: a -> (a -> b) -> b
(-:) x f = f x

height :: Num b => Zipper (a, b) -> b
height (Node (n, height) l c r, bs, flag) = height

focus :: Zipper a -> a
focus (Node a r s t, bs, flag) = a

list2tree :: [Integer] -> Zipper a -> Zipper a
list2tree (a:as) z = list2tree as $ tern2tree a z 
list2tree [] z = z

tern2tree :: Integer -> Zipper a -> Zipper a
tern2tree 1 z = z -: goLeft
tern2tree 0 z = z -: goCenter
tern2tree 2 z = z -: goRight

getFlag :: Zipper a -> Flag
getFlag (tree, bs, flag) = flag
