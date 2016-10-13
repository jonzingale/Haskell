module AlecLights where
import Control.Comonad
import Data.Bits

data Lights a = L a [a] deriving (Show)

initial = L 255 $ out_going 255

right (L a []) = L a []
right (L a (b:bs)) = L b bs

out_going :: Int -> [Int]
out_going bits = bit_operator <$> [0..7]
  where bit_operator = \n-> bits `xor` (shiftL 7 n) `mod` 256

next_state :: Int -> Lights Int
next_state a = L a $ out_going a

next_states :: Int -> [Lights Int]
next_states n = map next_state $ out_going n

mu :: Lights (Lights a) -> Lights a
mu (L (L a bs) cs) = L a $ fmap heads cs
  where heads (L a bs) = a

instance Functor Lights where
  fmap f (L a bs) = L (f a) $ map f bs

instance Applicative Lights where
  pure x = L x $ [x]
  (<*>) (L f fs) (L a bs) = L (f a) [g b | b<- bs, g<- fs] 

instance Monad Lights where
  m >>= f = mu $ f <$> m
  return = pure

lifted :: Lights (Lights Int)
lifted = fmap next_state initial

lifteds :: Lights [Lights Int]
lifteds = fmap next_states initial

monadic = initial >>= next_state