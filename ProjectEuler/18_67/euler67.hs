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
type EitherInt = Either String (Vector [Int])
data Tree = Empty | L Int | N Int Tree Tree

instance (Num a, Num b) => Num (a,b) where
  (+) (a,b) (c,d) = (a+c, b+d)

main = do
  csv <- BL.readFile "triangle.csv"
  let maxedTree = euler67.csvParser $ csv
  print maxedTree

csvParser file =
  let options = defaultDecodeOptions { decDelimiter = 32 } in
  let parsedCsv = decodeWith options NoHeader file :: EitherInt in
  reverse $ toList.fromRight empty $ parsedCsv

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
  (x:xs) * (y:ys) = (x*y) : ([x] * ys + xs * (y:ys))
  _ * _ = []
