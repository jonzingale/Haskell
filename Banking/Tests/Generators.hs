{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Tests.Generators where
import Test.QuickCheck.Monadic
import Test.Framework
import Data.Time.Calendar

data HeaderSelector = Header Int deriving (Show, Eq)
data TestDate = DateString String deriving (Show, Eq)
data TestCurrency =
  Currency { dollars :: Float, cents :: Float} deriving (Show, Eq)

instance Arbitrary HeaderSelector where
  arbitrary = do
    x <- choose (0, 3::Int)
    return.Header $ x

instance Arbitrary TestDate where
  arbitrary = do
    year <- choose (2000, 2100::Integer)
    month <- choose (1, 12::Int)
    day <- choose (1, 31::Int)
    let date = fromGregorian year month day
    return.DateString $ showGregorian date

instance Arbitrary TestCurrency where
  arbitrary = do
    dollars <- choose (-10^4, 10^4::Int)
    cents <- choose (0, 99::Int)
    return.Currency (fromIntegral dollars) $ (fromIntegral cents / 100)