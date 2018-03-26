
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- module Solution where
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

-- data Tree = L Int | N Int Tree Tree deriving (Show, Ord, Eq)

euler18 = maxTree.buildTree $ pyramid

csvParser file = 
  let options = defaultDecodeOptions { decDelimiter = 32 } in
  let parsedCsv = decodeWith options NoHeader file :: Either String (Vector [Int]) in
  toList.fromRight empty $ parsedCsv

main = do
  csv <- BL.readFile "triangle.csv"
  let maxedTree = buildAndMaxTree.csvParser $ csv
  print maxedTree

-- Still eats too much memory
buildAndMaxTree tower = let ([aj]:paired) = map (zip [0..]) tower in
  f aj paired
  where
    f (j,a) [] = [a]
    f (j,a) (b:bs) = max (f ((0,a) + b!!j) bs) (f ((0,a) + b!!(j+1)) bs)

---- work on tree traversal to as to throw away subtrees.
buildTree tree = let ([aj]:paired) = map (zip [0..]) tree in
  f aj paired
  where
    f (j,a) [] = L a
    f (j,a) (b:bs) = N a (f ((0,a)+b!!j) bs) (f ((0,a)+b!!(j+1)) bs)  

maxTree tree = maximum.f $ tree
  where
    f (L a) = [a]
    f (N a left right) = f left ++ f right 

instance (Num a, Num b) => Num (a,b) where
  (+) (a,b) (c,d) = (a+c, b+d)

-- zipper :: Zipper
-- zipper = (buildTree pyramid, [])

data Tree = Empty | L Int | N Int Tree Tree deriving (Show)
-- data Crumb = LeftCrumb Int Tree | RightCrumb Int Tree deriving (Show)

-- type BreadCrumbs = [Crumb]
-- type Zipper = (Tree, BreadCrumbs)

-- goLeft :: Zipper -> Zipper
-- goLeft (N x l r, bs) = (l, LeftCrumb x r:bs)

-- goRight :: Zipper -> Zipper
-- goRight (N x l r, bs) = (r, RightCrumb x l:bs)

-- goUp :: Zipper -> Zipper
-- goUp (t, LeftCrumb x r:bs) = (N x t r, bs)
-- goUp (t, RightCrumb x l:bs) = (N x l t, bs)
-- goUp (t, []) = (t, [])



pyramid :: [[Int]]
pyramid = [[75],
           [95,64],
           [17,47,82],
           [18,35,87,10], 
           [20,04,82,47,65],
           [19,01,23,75,03,34],
           [88,02,77,73,07,63,67],
           [99,65,04,28,06,16,70,92],
           [41,41,26,56,83,40,80,70,33],
           [41,48,72,33,47,32,37,16,94,29],
           [53,71,44,65,25,43,91,52,97,51,14],
           [70,11,33,28,77,73,17,78,39,68,17,57],
           [91,71,52,38,17,14,91,43,58,50,27,29,48], 
           [63,66,04,68,89,53,67,30,73,16,69,87,40,31],
           [04,62,98,27,23,09,70,98,73,93,38,53,60,04,23]]

-- solution by mvz
count [xs] = xs
count (xs:xss) = let cs = count xss in zipWith (+) xs (zipWith max (init cs) (tail cs))

-- Thinning Method
thin tree = let minnie = minimum.(filter (/= 0)).concat $ tree in
  map (map (subtr minnie)) tree
  where
    subtr n t | t == 0 = 0
              | otherwise = div t 2

thinning n = do
  let bark = (!!n).iterate thin $ pyramid

  (putStr.unlines.map show) bark
  -- putStrLn.(map show) $ bark