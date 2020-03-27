{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module InverseImage where

class Dom l where
  domain :: [l]

instance Dom Int where
  domain = [0..] ++ map negate [0..]

instance Dom Char where
  domain = ['a'..]

class InverseImage s b where
  invImg :: (s -> b) -> b -> [s]

instance (Dom s, Eq b) => (InverseImage s b) where
  invImg f b = [a | a <- domain, f a == b]

intEx :: [Int]
intEx = invImg (\ z -> z `mod` 10^5) 2

charEx :: String
charEx = invImg (\ c -> c:"hars") "ahars"