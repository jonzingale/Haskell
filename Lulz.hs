import Control.Monad
import Control.Applicative

newtype List a = List { getList :: [a] } deriving (Show)

instance Functor List where
	fmap f (List []) = List []
	fmap f (List xs) = List $ map f xs

instance Applicative List where
	pure n = List [n]
	(<*>) = ap

instance Monad List where
	return n = incl n
	m >>= f = (mu.fmap f) m

incl :: a -> List a
incl = pure

mu :: List(List a) -> List a
mu (List ls) = List $ concat.fmap cc $ ls
	where
		cc (List []) = []
		cc (List (x:xs)) = x : cc (List xs)
--mu mm = List $ concat.getList.fmap getList $ mm

m1 = incl 4
m2 = (incl.incl) 4
mix = List[List [5], List[6]]