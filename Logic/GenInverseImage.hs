{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}

{--
This may never work on account of the type variable b
in InverseImage not being specified for the use of domain in invImg.
--}
module GenInverseImage where

class InverseImage s b where
  invImg :: (s -> b) -> b -> [s]
  domain :: [s]

-- instance works because of explicit domain in comprehension
instance Eq b => InverseImage Char b where
  invImg f b = [a | a <- ['a'..], f a == b]
  domain = ['a'..]

-- instance fails because of implicit domain in comprehension
instance Eq x => InverseImage Int x where
  invImg f b = [a | a <- domain, f a == b]
  domain = [0..] ++ map negate [0..]

charEx2 = invImg (\ c -> c:"hars") "ahars"