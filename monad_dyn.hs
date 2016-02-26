module MonadDyn where
import System.Random
import Data.Char

-- how about Dyn!
-- Root: Dyn -> Cycles

type N = Integer

it = TT 4 6 

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


{-- 
How do I implement Dyns here?

G is faithful iff for any d∈D the morphism εd is an epimorphism;
G is full iff for any d∈D the morphism εd is a split monomorphism

So, for example, if G is full, and for some d0∈D the morphism 
εd0:FGd0→d0 does not have a left inverse, then ε is not a counit.

Finally, another interesting property you may find useful:
If one of the functors F, G is full, then the natural transformation 
Gε is invertible.

ηG(d)=(G(εd))−1
--}