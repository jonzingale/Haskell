module MonadicDom where
import Control.Monad.Writer

{--
The goal is functionality for treating DOM Manipulation monadically, a limiting
case where side-effects are encapsulated and pure functions can roam free.
--}

data Tree = Leaf Int | Tree Tree Tree deriving (Show, Eq)

instance Semigroup Tree where
  (<>) l r = Tree l r

instance Monoid Tree where
  mempty = Leaf 0

t1 :: Tree
t1 = Tree (Tree (Leaf 3) (Leaf 4)) (Tree (Leaf 5) (Leaf 4))

t2 :: Tree
t2 = Tree (Tree (Leaf 3) (Leaf 4)) (Leaf 4)

applyElem :: Monoid m => (a, m) -> (a -> (b, m)) -> (b, m)
applyElem (x, tree) f =
  let (y, newTree) = f x in (y, tree <> newTree)

apEx1 = applyElem (2, "baller") $ \x -> (x + 3, "Dang")
apEx2 = applyElem (3, Leaf 3) $ \x -> (x + 2, t1)

{--
To establish Tree as a monoid, I will need to define a meaningful notion
of mappend. How would a left element know how to mappend to a right element?
--}

