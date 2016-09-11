import Control.Monad
import Control.Applicative

newtype List a = List { getList :: [a] } deriving (Show)

instance Functor List where
	fmap f (List []) = List []
	fmap f (List xs) = List $ map f xs

instance Applicative List where
	pure n = List [n]
	(<*>) = ap

incl :: a -> List a
incl = pure

-- not quite right as id :: a -> a not a -> ma
-- yet i can pass id as f to >>=
mu :: List(List a) -> List a
mu mm = List $ concat.getList.fmap getList $ mm

instance Monad List where
	return n = incl n
	m >>= f = (mu.fmap f) m

m1 = incl 4
m2 = (incl.incl) 4
mix = List[List [5], List[6]]