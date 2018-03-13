module TraversableTree where

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Show, Eq)

instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Leaf x) = Leaf $ f x
  fmap f (Node l k r) = Node (fmap f l) (f k) (fmap f r)

instance Foldable Tree where
  foldr f b Empty = b
  foldr f b (Leaf x) = f x b
  foldr f z (Node l k r) = foldr f (f k (foldr f z r)) l

instance Applicative Tree where
  pure x = Leaf x
  (<*>) (Leaf f) (Leaf x) = Leaf $ f x
  (<*>) (Node fs g hs) (Node l b r) = Node (fs <*> l) (g b) (hs <*> r)

instance Traversable Tree where
  traverse f Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Node l k r) = Node <$> traverse f l <*> f k <*> traverse f r

tree = Node (Node (Leaf 1) 2 (Leaf 3)) 4 (Node (Leaf 6) 5 (Leaf 7))
sumTree = (+) <$> tree <*> tree
idTree = (traverse pure tree)::Tree (Tree Integer)
propagate = traverse (\x -> Node (Leaf x) x (Leaf x)) tree