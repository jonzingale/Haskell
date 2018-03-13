module InfiniteList where
import Text.Printf

data InfList a = IL {left :: [a], focus :: a, right :: [a]}

instance Show a => Show (InfList a) where
   show (IL a b c) = printf format (ff reverse a) (show b) (ff id c)
    where
      format = "[..%s { %s } %s..]\n"
      ff f = unwords.(map show).f.(take 3)

shiftLeft :: InfList a -> InfList a
shiftLeft (IL (a:as) b cs) = IL as a (b:cs)

shiftRight :: InfList a -> InfList a
shiftRight (IL as b (c:cs)) = IL (b:as) c cs

integers :: InfList Integer
integers = IL (map negate [1..]) 0 [1..]

limit :: Int -> InfList a -> InfList a
limit n (IL a b c) = IL (take n a) b (take n c)

instance Functor InfList where
  fmap f (IL a b c ) = IL (map f a) (f b) (map f c)

instance Applicative InfList where
  pure x = IL (repeat x) x (repeat x)
  (<*>) (IL fs g hs) (IL as b cs) = IL (fs <*> as) (g b) (hs <*> cs)

instance Foldable InfList where -- ‘foldMap’?
  foldr f base infList = foldr f base $ folds infList 0
    where
      folds (IL xs x []) _ = (x:xs) 
      folds (IL [] x xs) _ = (x:xs) 
      folds (IL as b (c:cs)) 0 = b : folds (IL as c cs) 1
      folds (IL (a:as) b cs) 1 = b : folds (IL as a cs) 0


{--
Todo:
Listable
Sortable
Traversable
Monad
--}