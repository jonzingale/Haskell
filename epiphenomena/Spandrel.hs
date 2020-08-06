module Spandrel where
import Data.Bifunctor (second)
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

-- builds from KeySortable class and shapes
buildBlocks :: [Shape] -> [Pair Shape Color]
buildBlocks shapes = [second f $ diag shape | shape <- shapes]
  where
    f Circle = Red
    f Square = Yellow
    f Triangle = Blue

soberSort :: IO [Color]
soberSort = do
  let shapes = [Circle, Square, Triangle, Square, Circle, Triangle, Square]
  let blocks = buildBlocks shapes
  let sortedColors = sort blocks
  return $ map pr2 sortedColors

keyShuffle :: IO [Color]
keyShuffle = do
  sober <- soberSort
  let shuffledColors = shuffle sober :: [Pair Int Color]
  return $ map pr2 shuffledColors
