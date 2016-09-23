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

data Lulz a = Lulz (List (List a)) deriving (Show)

instance Functor Lulz where
  fmap f (Lulz (List [List []])) = Lulz $ List [List []]
  fmap f (Lulz (List [List xs])) = Lulz $ List [List (map f xs)]

instance Applicative Lulz where
  pure = η
  (<*>) = ap

instance Monad Lulz where
  return = pure
  m >>= f = μ.fmap f $ m

η :: a -> Lulz a
η = \x -> Lulz $ incl . incl $ x

μ :: Lulz(Lulz a) -> Lulz a
μ (Lulz(List[List [lulz]])) = lulz

m1 = incl 4
m2 = incl.incl $ 4
mix = List[List [5], List[6]]
lol = Lulz m2
it = η . η $ 3
