module Spandrel where
import Sortable

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

soberSort = do -- builds from KeySortable class and shapes
  let blocks = [Circle, Square, Triangle, Square, Circle, Triangle, Square]
  let isomorph = map (rmap f) $ map diag blocks ::[Pair Shape Color]
  return $ (map pr2).sort $ isomorph
  where
    f Circle = Red
    f Square = Yellow
    f Triangle = Blue

keyShuffle = do -- builds from KeySortable class and Sober sorted colors
  sober <- soberSort
  let colors = map pr2 $ (shuffle sober :: [Pair Int Color])
  print colors

{-- Todo:
Build out number theory example with composites mapped to divisibility
restricted shapes. How should the fibers be constructed?
--}