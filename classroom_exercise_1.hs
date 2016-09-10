import Control.Monad

data Btree a = Leaf a | Fork (Btree a) (Btree a) | EmptyTree
		 deriving (Show , Eq)

instance Functor Btree where
 	fmap f EmptyTree = EmptyTree
 	fmap f (Leaf n) = Leaf (f n)
 	fmap f (Fork l r) = Fork (fmap f l) (fmap f r)

--iota :: Int -> Btree Int
--iota n = Leaf n

unit :: Int -> Btree Int
unit n = Fork EmptyTree (Leaf n)


powerset xs = filterM (\x -> [True, False]) xs
--class Monad m where
--	return :: a -> ma
--	(>>=) :: ma -> (a -> mb) -> mb

--instance Monad Btree where
	--iota n = Btree n
	--n >>= f 