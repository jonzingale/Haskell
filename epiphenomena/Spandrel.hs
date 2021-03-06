module Spandrel where
import Sortable
import Shape

type ColorBlocks = [Pair Shape Color]
type Colors = [Color]

-- builds from KeySortable class and shapes
buildBlocks :: Shapes -> ColorBlocks
buildBlocks shapes = [second f $ diag shape | shape <- shapes]
  where
    f Circle = Red
    f Square = Yellow
    f Triangle = Blue

soberSort :: IO Colors
soberSort = do
  let shapes = take 50 shapeGen
  let blocks = buildBlocks shapes
  let sortedColors = sort blocks
  return $ map pr2 sortedColors

keyShuffle :: IO Colors
keyShuffle = do
  sober <- soberSort -- begin with ordered blocks
  let shuffledColors = shuffle sober :: [Pair Int Color]
  return $ map pr2 shuffledColors
