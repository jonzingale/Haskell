module Shape where

data Color = Red | Yellow | Blue deriving (Show, Eq)
data Shape = Circle | Square | Triangle deriving (Show, Eq)

instance Ord Shape where
  compare Circle _ = LT
  compare _ Circle = GT
  compare Triangle _ = LT
  compare _ Triangle = GT
  (<=) x y | x == y = True
           | otherwise = compare x y == LT
  (>) x y = not (x <= y)