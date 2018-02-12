module ListsAsNumbers where

{--
In this module, I aim to extend lists of numbers to the number class
by considering the 0th index to be the 0th place value of a number.
of course, this really only works for single digit list values.
an inclusion method is likely a good idea.

satisfactory methods for Num Class: + - * abs signum fromInteger

1) Given listify how do we write an inclusion methods
2) How do we write Num methods?
3) can we generalize the binary operations?
4) What about negative numbers?
--}

numl = N [1,2,3,4]
munl = N [-1,-2,-3,-4]

data NumList = N [Integer] | BadNumList deriving (Show, Ord, Eq)

incl n | n < 0 = N $ map (* (-1)) $ listify (-n)
       | otherwise = N $ listify n
  where
    listify 0 = []
    listify n = (listify (div n 10)) ++ [mod n 10]

binOp bin (N ns) (N ms) = N (f bin ns ms)
  where
    f bin [] ys = ys
    f bin xs [] = xs
    f bin (x:xs) (y:ys) =  bin x y : f bin xs ys

instance Num NumList where
  numN * numM = binOp (*) numN numM
  numN + numM = binOp (+) numN numM
  numN - numM = binOp (-) numN numM

  fromInteger n = incl n

  signum (N ns) | all (== 0) ns = N [0]
                | all (>= 0) ns = N [1]
                | all (<= 0) ns = N [-1]
                | otherwise = BadNumList

  abs (N ns) | all (>= 0) ns = N ns
             | all (<= 0) ns = N $ map (* (-1)) ns
             | otherwise = BadNumList
