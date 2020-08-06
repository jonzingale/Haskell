module Spandrel where
import Data.Bifunctor (second)
import Sortable
import Shape

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
