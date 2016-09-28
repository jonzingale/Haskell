module OnTv where
import Prelude hiding (Monad,(>>=))

{-- 
    The goal here is to flesh out
    an idea about being on tv.
    Namely, once one is on tv, one
    is always on tv.
--}

data Tv s = Tv s deriving (Show)

instance Functor Tv where
  fmap f (Tv s) = Tv (f s)

instance Monad Tv where
  unit b = Tv b
  join (Tv (Tv a)) = Tv a
  x >>= f = join $ fmap f x

class Functor m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  join :: m (m a) -> m a
  unit :: a -> m a

-- Helpers

camera :: String -> Tv String
camera person = Tv person

hipster = Tv "dude"

main = putStrLn.show $ hipster >>= camera >>= camera