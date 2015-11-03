module Monad where
import System.Random
import Data.Char

-- Todo: Reverse the arrows all the way down
-- Product x -> TT (x,y)
-- Set x -> P (x)

type N = Integer

it = TT 4 6 

--data TT s = Nil
--						| Pr { left :: TT s,
--										value :: s,
--										right :: TT s
--									}

data TT s = TT s s
pr1 (TT a b) = a
pr2 (TT a b) = b

instance Show a => Show (TT a) where
  show (TT a b) = show (a,b)

instance Functor TT where
  fmap f (TT s t) = TT (f s) (f t)

class Functor m => Gonad m where
  (>>>=) :: m a -> (a -> m b) -> m b
  unit :: a -> m a
  mult :: m (m a) -> m a
  x >>>= f = mult $ fmap f x

instance Gonad TT where
  unit b = TT b b
  mult (TT (TT a b) (TT c d)) = TT a d
  --mult (TT (TT a b) (TT c d)) = TT (pr1 (TT a b)) (pr2 (TT a b))


-- note the f :: a -> m b
-- :t pr1(lit >>>= unit)
-- :t lit

-- god only knows.
-- eval: FG --> ID
-- counit :: TT (TT a) -> a

-------------

--instance Comonad U where
--  cojoin a = U (tail $ iterate left a) a (tail $ iterate right a)
--  coreturn (U _ b _) = b

