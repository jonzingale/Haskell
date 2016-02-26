module Garbage where
import System.Random
import Control.Applicative
import Control.Monad
import Data.Char

type N = Integer
type Thing = String

--  The goal here is to flesh out
--  some ideas about garbage.

-- HOW DO I DERIVE SHOW FOR THE MONAD?

-- how do I verify the identity?
-- id: Gx -unit-> GFGx -counit-> Gx

--  where the second pair is used up'd ness
--	F = A -> (A,*)
--	G = (A,T) -> A

it :: G Thing
it = G "thing" (Entropy 5)

data Entropy = Nil | Entropy Integer deriving (Show)
data G s = G s Entropy deriving (Show)

counit (G s n) = G s (Entropy 10)

instance Applicative G where
  pure = unit
  (<*>) = ap

instance Functor G where
  fmap f (G s t) = G (f s) t
 --example: fmap (++ "y") it

class Functor m => Garbage m where
  (>>>=) :: m a -> (a -> m b) -> m b
  unit :: a -> m a
  mult :: m (m a) -> m a
  x >>>= f = mult $ fmap f x

instance Garbage G where
  unit b = G b Nil
  mult (G (G a t) _) = G a t

instance Monad G where
  return b = G b Nil
  (G a t) >>= f = join $ fmap f (G a t)
--  example: it >>= (return)

{--
instance Functor G where
  fmap = liftM

instance Applicative G where
  pure = return
  (<*>) = ap

instance Monad G where
  return b = G b Nil
  (G a t) >>= f = join $ fmap f (G a t)
--  example: it >>= (return)
--}




--USEFUL EXAMPLE
--instance Show a => Show (State a) where
  --show (State f) = show [show i ++ " => " ++ show (f i) | i <- [0..3]]