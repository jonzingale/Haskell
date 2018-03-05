{-# LANGUAGE OverloadedStrings #-}
-- https://hackage.haskell.org/package/cassava-0.5.1.0#readme
-- https://hackage.haskell.org/package/cassava-0.5.1.0/docs/Data-Csv.html

-- import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Csv

data Person = Person
    { name   :: !String
    , salary :: !Int
    }

instance FromNamedRecord Person where
    parseNamedRecord r = Person <$> r .: "name" <*> r .: "salary"

instance ToNamedRecord Person where
    toNamedRecord (Person name salary) = namedRecord [
        "name" .= name, "salary" .= salary]

sarahJ = toNamedRecord $ Person "Sarah J" (10^6)

main :: IO ()
main = do
    csvData <- BL.readFile "salaries.csv"
    case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, v) -> V.forM_ v $ \ p ->
            putStrLn $ name p ++ " earns " ++ show (salary p) ++ " dollars"

type FileStub = String

parseCsv :: FileStub -> IO()
parseCsv file_stub = do
  csvData <- BL.readFile file_stub -- :: BL.ByteString
  case decodeByName csvData of
      Left err -> putStrLn err
      Right (_, v) -> V.forM_ v $ \ p ->
          putStrLn $ name p ++ " earns " ++ show (salary p) ++ " dollars"

-- writing data
myData :: [(String, String)]
myData = [("name","salary"), ("John Doe","50000"), ("Jane Doe", "60000")]

writeData csvData = BL.writeFile "letters.csv" $ encode csvData

