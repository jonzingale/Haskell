import Control.Monad

data List a = List [a] deriving (Show)

instance Functor List where
  fmap f (List []) = List []
  fmap f (List xs) = List $ map f xs

instance Applicative List where
  pure n = List [n]
  (<*>) = ap

instance Monad List where
  return = pure
  m >>= f = (mu.fmap f) m

incl :: a -> List a
incl = pure

mu :: List(List a) -> List a
mu (List ls) = List $ concat.fmap cc $ ls
  where
    cc (List []) = []
    cc (List (x:xs)) = x : cc (List xs)

m1 = incl 4
m2 = (incl.incl) 4
mix = List[List [5], List[6]]