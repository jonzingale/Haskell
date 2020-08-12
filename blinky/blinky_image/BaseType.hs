module BaseType where

instance Monoid Int where
  mappend = (+)
  mempty = 0

instance Monoid Bool where
  mappend = (||)
  mempty = True