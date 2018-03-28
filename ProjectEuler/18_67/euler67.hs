{-# OPTIONS_GHC -Wno-missing-methods #-}

import qualified Data.ByteString.Lazy as BL
import Data.Vector (Vector, empty, toList)
import Data.Either.Extra (fromRight)
import Data.Csv

type EitherInt = Either String (Vector [Int])

csvParser file =
  let options = defaultDecodeOptions { decDelimiter = 32 } in
  let parsedCsv = decodeWith options NoHeader file :: EitherInt in
  toList.fromRight empty $ parsedCsv

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

main = do
  csv <- BL.readFile "triangle.csv"
  let maxedTree = euler67.reverse.csvParser $ csv
  print maxedTree

euler67 [as] = head as
euler67 (as:bs:css) =
  let ff = as + bs in
  let gg = tail $ as + (0:bs) in
  euler67 ([max c1 c2 | (c1, c2)<- zip ff gg]:css)

instance Num a => Num [a] where
  fromInteger n = [fromInteger n]
  (x:xs) + (y:ys) = (x + y) : (xs + ys)
  xs + [] = 0
  [] + ys = 0
