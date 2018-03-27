module HaskellJoe where

doubleMe :: Integer -> Integer
doubleMe x = x + x

doubleUs :: (Integer -> Integer) -> Integer
doubleUs f = f 3