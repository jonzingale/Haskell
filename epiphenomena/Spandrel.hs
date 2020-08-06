module Spandrel where
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
  let shapes = take 50 shapeGen
  let blocks = buildBlocks shapes
  let sortedColors = sort blocks
  return $ map pr2 sortedColors

keyShuffle :: IO [Color]
keyShuffle = do
  sober <- soberSort -- begin with ordered blocks
  let shuffledColors = shuffle sober :: [Pair Int Color]
  return $ map pr2 shuffledColors
