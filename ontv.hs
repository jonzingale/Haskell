module OnTV where
import System.Random
import Data.Char

type N = Integer

-- The goal here is to flesh out
-- some ideas about being on tv.

--data Tree a = Nil 
--            | Node { left  :: Tree a,
--                     value :: a,
--                     right :: Tree a }

--three_number_tree :: Tree Integer
--three_number_tree = Node (Node Nil 1 Nil) 2 (Node Nil 3 Nil)

it = TV 4

--data TV s = Nil | Famous { me :: TV s}
--						| Pr { left :: TV s,
--									 value :: s,
--									 right :: TV s
--									}

data TV s = TV s
--pr1 (TV a b) = a
--pr2 (TV a b) = b

instance Show a => Show (TV a) where
  show (TV a) = show a

instance Functor TV where
  fmap f (TV s) = TV (f s)

class Functor m => Gonad m where
  (>>>=) :: m a -> (a -> m b) -> m b
  unit :: a -> m a
  mult :: m (m a) -> m a
  x >>>= f = mult $ fmap f x

instance Gonad TV where
  unit b = TV b
  mult (TV (TV a)) = TV a


-- note the f :: a -> m b
-- :t pr1(lit >>>= unit)
-- :t lit
