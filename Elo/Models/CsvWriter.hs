{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.CsvWriter where
import Models.Player (persons, Player(Player))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Csv (FromRecord, ToRecord, encDelimiter, defaultEncodeOptions)
import Data.Vector (Vector, empty, toList)
import Data.Csv.Incremental
import Data.Char (ord)
import GHC.Generics

-- writeToFile "SpringRank/data/players.csv" persons => IO ()
writeToFile :: ToRecord a => FilePath -> [a] -> IO ()
writeToFile file ds = do
  BL.writeFile file $
    encodeWith myOptions $
    foldMap encodeRecord ds
  where
    myOptions = defaultEncodeOptions {
        encDelimiter = fromIntegral (ord ' ')
      }
