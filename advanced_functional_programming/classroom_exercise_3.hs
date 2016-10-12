-- State Monad

-- runState :: State s a -> s -> (a, s)
newtype State s a = State { runState :: s -> (a,s) }

get = State $ \s -> (s,s) -- return
put s = State $ \_ -> ((),s)

fromStoAandS :: Int -> (String, Int)
fromStoAandS c | c `mod` 5 == 0 = ("foo",c+1)
               | otherwise = ("bar",c+1)

stateIntString :: State Int String
stateIntString = State fromStoAandS

this = runState stateIntString 7
that = runState stateIntString 1

data Tree a = Unary (Tree a) | Binary (Tree a) (Tree a)
    | Ternary (Tree a) (Tree a) (Tree a) | A a | G a | C a | T a
    deriving (Show, Eq)

foo :: Tree ()
foo = Ternary (Binary (A ()) (G ())) (C ()) (Ternary (G ()) (G ())
              (Binary (Unary (C ())) (T ())))

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

stateIntTree :: State Int (Tree ())
stateIntTree = State fromStoTreeandS
-- (runState (State fromStoTreeandS)) 3

treeBlanktoInt :: Tree a -> Tree Int
treeBlanktoInt tree = fmap (\x-> 0) tree

updateG tree | tree == G 0 = G 1
             | otherwise = tree

-- label :: Tree a -> Tree Int
-- label Tree t | 
