{-# LANGUAGE DeriveGeneric #-}

module Text where
import qualified Data.ByteString.Lazy as BL

filename = ""

main = do  
  google <- BL.readFile "./google.csv"

