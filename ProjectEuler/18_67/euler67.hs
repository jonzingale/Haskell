{-# OPTIONS_GHC -Wno-missing-methods #-}

import qualified Data.ByteString.Lazy as BL
import Data.Vector (Vector, empty, toList)
import Data.Either.Extra (fromRight)
import Data.List
import Data.Csv
{--
By starting at the top of the triangle below and moving to
adjacent numbers on the row below, the maximum total from
top to bottom is 23.

3
7 4
2 4 6
8 5 9 3

That is, 3 + 7 + 4 + 9 = 23.
--}

csvParser file = 
  let options = defaultDecodeOptions { decDelimiter = 32 } in
  let parsedCsv = decodeWith options NoHeader file :: Either String (Vector [Int]) in
  toList.fromRight empty $ parsedCsv

main = do
  csv <- BL.readFile "triangle.csv"
  let maxedTree = buildAndMaxTree.csvParser $ csv
  print maxedTree

-- Still eats too much memory
data Tree = Empty | L Int | N Int Tree Tree deriving (Show)

buildAndMaxTree tower = let ([aj]:paired) = map (zip [0..]) tower in
  f aj paired
  where
    f (j,a) [] = [a]
    f (j,a) (b:bs) = max (f ((0,a) + b!!j) bs) (f ((0,a) + b!!(j+1)) bs)

instance (Num a, Num b) => Num (a,b) where
  (+) (a,b) (c,d) = (a+c, b+d)