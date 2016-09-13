import Control.Monad

data Btree a = Leaf a | Fork (Btree a) (Btree a)
		 deriving (Show , Eq)

instance Functor Btree where
 	fmap f (Leaf n) = Leaf (f n)
 	fmap f (Fork l r) = Fork (fmap f l) (fmap f r)

growTree 0 tree = tree
growTree n tree = Fork (growTree (n-1) tree) (growTree (n-1) tree) 

unit = Leaf (Just 1)

fromSeed n = growTree n unit
treeHeight = (+1).floor.(logBase 2).(+(-1)).fromIntegral

prune 0 tree = tree
prune 1 (Fork (Leaf a) leaf_b) = Fork (Leaf Nothing) leaf_b
prune 2 (Fork (Leaf a) (Leaf b)) = (Fork (Leaf a) (Leaf b))
prune n (Fork tree_a tree_b) | odd n = Fork (prune (n `div` 2) tree_a)
																						(prune (n `div` 2 + 1) tree_b)
														 | otherwise = Fork (prune (n `div` 2) tree_a) 
														 										(prune (n `div` 2) tree_b)


iota 1 = unit
iota n = prune n $ (growTree.treeHeight) n $ unit

--class Monad m where
--	return :: a -> ma
--	(>>=) :: ma -> (a -> mb) -> mb

--instance Applicative Btree where
--	pure = iota

--instance Monad Btree where
  --return = iota
  --m >>= f = (mu.fmap f) m