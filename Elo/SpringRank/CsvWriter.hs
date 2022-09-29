{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module SpringRank.CsvWriter where
import Data.ByteString (ByteString, hGetSome, empty)
import qualified Data.ByteString.Lazy as BL
import Data.Csv (FromRecord, ToRecord, encDelimiter, defaultEncodeOptions)
import Data.Csv.Incremental
import Data.Char (ord)
import GHC.Generics

data Player = Player
  { idx :: !Int,
    name :: !ByteString,
    goRank :: !ByteString,
    elo :: !Double,
    rank  :: !Double
  } deriving (Show, Eq, Generic)

instance FromRecord Player
instance ToRecord Player

persons :: [Player]
persons = [
  Player 0 "Billy" "10.0k" 1100 0,
  Player 1 "Kyle" "9.0k" 1200 0
  ]

writeToFile :: [Player] -> IO ()
writeToFile ps = do
  BL.writeFile "SpringRank/data/players.csv" $
    encodeWith myOptions $
    foldMap encodeRecord ps
  where
    myOptions = defaultEncodeOptions {
        encDelimiter = fromIntegral (ord ' ')
      }