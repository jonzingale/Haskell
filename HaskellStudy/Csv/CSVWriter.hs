module CSVWriter where
import Data.Vector (Vector, empty, toList)
import Data.Either.Extra (fromRight)
import qualified Data.Csv as CSV
import Data.List

import Data.List.Split

helloFile :: String -> IO ()
helloFile str = appendFile "./tmp.csv" str

readThenWriteCsv file = do
  csv <- readFile file
  let rows = splitOn "\r" csv
  print rows

  helloFile $ intercalate "\r" rows
