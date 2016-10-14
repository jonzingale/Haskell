import Control.Monad.State

-- runState :: State s a -> s -> (a, s)
-- newtype State s a = State { runState :: s -> (a,s) }

fromStoAandS :: Int -> (String, Int)
fromStoAandS c | c `mod` 5 == 0 = ("foo",c+1)
               | otherwise = ("bar",c+1)

data Tree a = Unary (Tree a) | Binary (Tree a) (Tree a)
              | Ternary (Tree a) (Tree a) (Tree a)
              | A a | G a | C a | T a deriving (Show, Eq)

foo :: Tree ()
foo = Ternary (Binary (A ()) (G ())) (C ()) (Ternary (G ()) (G ())
              (Binary (Unary (C ())) (T ())))

food :: Tree Int
food = Ternary (Binary (A 0) (G 0)) (C 0) (Ternary (G 0) (G 0)
              (Binary (Unary (C 0)) (T 0)))

instance Functor Tree where
  fmap f (Ternary a b c) = Ternary (fmap f a) (fmap f b) (fmap f c)
  fmap f (Binary a b) = Binary (fmap f a) (fmap f b)
  fmap f (Unary a) = Unary (fmap f a) 
  fmap f (A n) = A (f n)
  fmap f (G n) = G (f n)
  fmap f (C n) = C (f n)
  fmap f (T n) = T (f n)

fromStoTreeandS :: Int -> (Tree (), Int)
fromStoTreeandS c = (foo, c+1)

treeBlanktoInt :: Tree a -> Tree Int
treeBlanktoInt tree = fmap (\x-> 0) tree