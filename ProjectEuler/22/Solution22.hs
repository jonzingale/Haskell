{--
Names scores
Problem 22

Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.

For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.

What is the total of all the name scores in the file?
--}

module Solution22 where
import Data.Char
import Names

-- :set +s for testing run time speed

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = (qsort.smalls) (x:xs) ++ [x] ++ (qsort.bigs) (x:xs)
  where
    smalls (a:as) = [t | t<-as, t < a]
    bigs   (a:as) = [t | t<-as, t > a]

charToInt :: Char -> Integer
charToInt alpha =  fromIntegral $ ord alpha - 64

nameToInt :: String -> Integer
nameToInt name = sum.map charToInt $ name

challenge22 :: Integer
challenge22 = sum [i * nameToInt name | (i,name) <- zip [1..] (qsort names)]