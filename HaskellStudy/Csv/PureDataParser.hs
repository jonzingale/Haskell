{-# LANGUAGE DeriveGeneric #-}

module PureDataParser where
import qualified Data.ByteString.Lazy as BL
import Data.Vector (Vector, empty, toList)
import Data.Either.Extra (fromRight)
import Text.Printf
import Data.List
import Data.Csv
import Data.Char

canvasFormat = "%s %s %s %s data:%s\n"
connectFormat = "%s %s src:%d outlet:%d tar:%d inlet:%d\n"
floatAtomFormat = "%s %s %s len:%d data:%s\n"
restoreFormat = "%s %s %s name: %s\n"
objFormat = "%s %s %s name: %s\n"
msgFormat = "%s %s %s val:%d\n"

type EitherPureData = Either String (Vector [String])

data PureDataObject = Canvas { nodeType :: String, objTyp :: String, coords1 :: (Int, Int), coords2 :: (Int, Int), datum :: [String] } |
                      Object { nodeType :: String, objTyp :: String, coords :: (Int, Int), name :: String } |
                      Connect { nodeType :: String, objTyp :: String, source :: Int, outletNum :: Int, target :: Int, inletNum :: Int } |
                      FloatAtom { nodeType :: String, objTyp :: String, coords :: (Int, Int), lenN :: Int, datum :: [String] } |
                      Restore { nodeType :: String, objTyp :: String, coords :: (Int, Int), name :: String } |
                      MSG { nodeType :: String, objTyp :: String, coords :: (Int, Int), val :: Int } |
                      Empty

instance Show PureDataObject where
  show (Canvas n o coords1 coords2 ds) = printf canvasFormat n o (show coords1) (show coords2) (show ds)
  show (Object n o coords ds) = printf objFormat n o (show coords) ds
  show (Connect n o src out tar inlet) = printf connectFormat n o src out tar inlet
  show (FloatAtom n o coords len ds) = printf floatAtomFormat n o (show coords) len (show ds)
  show (Restore n o coords name) = printf restoreFormat n o (show coords) name
  show (MSG n o coords val) = printf msgFormat n o (show coords) val
  show Empty = "Empty\n"

toDatum ::[String] -> PureDataObject
toDatum (node:obj:x:y:dats) = case obj of
  "canvas" -> Canvas node obj (read x, read y) (read(dats!!0), read(dats!!1)) (drop 2 dats)
  "connect" -> Connect node obj (read x) (read y) (read(dats!!0)) (read.init $ dats!!1)
  "floatatom" -> FloatAtom node obj (read x, read y) (read(dats!!0)) (tail dats)
  "restore" -> Restore node obj (read x, read y) (init $ dats!!1)
  "msg" -> MSG node obj (read x, read y) (read.init $ dats!!0)
  "obj" -> Object node obj (read x, read y) (init.unwords $ dats)
  _ -> Empty -- throws away unexpected data

pureDataRecords = (map toDatum).toList.(fromRight empty).parseCsv
  where
    parseCsv csv = decodeWith delimiter NoHeader csv :: EitherPureData
    delimiter = defaultDecodeOptions { decDelimiter = fromIntegral (ord ' ') }

main = do  
  pureData <- BL.readFile "examplePD.pd"
  let pureRecords = pureDataRecords pureData
  print pureRecords
