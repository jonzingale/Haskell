module Heyting where

{--
The goal is to stub out Heyting so that it behaves like Bool.
Rather than just True and False, graded truth.
--}

data Heyting a = H Int a deriving Show

instance Eq a => Eq (Heyting a) where
  (==) (H n v) (H m w) = and [n == m, v == w]

instance Eq a => Ord (Heyting a) where
  (<=) (H n v) (H m w) = n <= m
  (>=) (H n v) (H m w) = n >= m

-- type dependency?
-- class Heyting a where
  -- insert :: a -> H z a