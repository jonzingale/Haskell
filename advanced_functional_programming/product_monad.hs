import Control.Monad
import Prelude hiding (product)

data Pair a = CxC (a, a) deriving (Show, Eq)
data Product a = TT a a deriving (Show, Eq) 

instance Functor Pair where
  fmap f (CxC (x, y)) = CxC (f x, f y)

instance Functor Product where
  fmap f (TT x y) = TT (f x) (f y)
  -- Alternatively,
  -- fmap f axb = product $ fmap f $ CxC (pr1 axb, pr2 axb)

pr1 :: Product a -> a
pr1 (TT x y) = x

pr2 :: Product a -> a
pr2 (TT x y) = y

eta :: a -> Product a
eta x = product.diagonal $ x

diagonal :: a -> Pair a
diagonal x = CxC (x, x)

product :: Pair a -> Product a
product (CxC (a,b)) = TT a b

mu :: Product a -> Pair a
mu axb = CxC (pr1 axb, pr2 axb)

counit :: Pair (Product a) -> Pair a
counit (CxC (TT a b, TT c d)) = CxC (a, d)

-- adjunction identities. GεηG / εFFη
_GεηG = TT 3 4 == (product.counit.diagonal) (TT 3 4)
_εFFη =  CxC (2,2) == (counit.fmap eta) (CxC (2,2))

-- Alternatively,
-- _GεηG = (pr1.eta) 3 == 3 && 3 == (pr2.eta) 3
-- _εFFη = CxC (3,3) == (counit.diagonal.eta) 3

instance Applicative Product where
  pure = eta
  (<*>) = ap

instance Monad Product where
  return = eta
  m >>= f = TT ((pr1.pr1) fm) ((pr2.pr2) fm)
    where fm = fmap f m 

lifted :: a -> Product (Product a)
lifted = eta . eta

this :: Product Int
this = TT 2 3

that :: Product (Product Int)
that = TT (TT 2 3) (TT 4 5)

fun :: Int -> Product Int
fun = \x -> TT (x+2) (x+4)

gun :: Int -> Product Int
gun = \x -> TT (x-2) (x-4)

main = putStrLn.show $ this >>= fun

left_id = (return 3 >>= fun) == fun 3
right_id = (this >>= return) == this
associativity = (this >>= fun >>= gun) == (this >>= (\x-> fun x >>= gun))