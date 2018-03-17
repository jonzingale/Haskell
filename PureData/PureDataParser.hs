{-# LANGUAGE DeriveGeneric #-}

module PureDataParser where
import Data.List.Split
import Text.Printf
import Data.Char

connectFormat = "%s %s src:%d outlet:%d tar:%d inlet:%d\n"
floatAtomFormat = "%s %s coords:%s len:%d data: %s\n"
canvasFormat = "%s %s coord1:%s coords2:%s name: %s\n"
restoreFormat = "%s %s %s name: %s\n"
objFormat = "%s %s %s data: %s\n"
msgFormat = "%s %s %s val:%d\n"

data PureDataObject =
  Connect   { nodeType :: String, objType :: String, source :: Int, outletNum :: Int, target :: Int, inletNum :: Int } |
  Canvas    { nodeType :: String, objType :: String, coords1 :: (Int, Int), coords2 :: (Int, Int), name :: String } |
  FloatAtom { nodeType :: String, objType :: String, coords :: (Int, Int), lenN :: Int, datum :: String } |
  Restore   { nodeType :: String, objType :: String, coords :: (Int, Int), name :: String } |
  Object    { nodeType :: String, objType :: String, coords :: (Int, Int), name :: String } |
  MSG       { nodeType :: String, objType :: String, coords :: (Int, Int), val :: Int } |
  Empty deriving Eq

instance Show PureDataObject where
  show (Canvas n o coords1 coords2 ds) = printf canvasFormat n o (show coords1) (show coords2) (show ds)
  show (FloatAtom n o coords len ds) = printf floatAtomFormat n o (show coords) len (show ds)
  show (Connect n o src out tar inlet) = printf connectFormat n o src out tar inlet
  show (Restore n o coords name) = printf restoreFormat n o (show coords) name
  show (Object n o coords ds) = printf objFormat n o (show coords) ds
  show (MSG n o coords val) = printf msgFormat n o (show coords) val
  show Empty = "Empty\n"

toDatum :: String -> PureDataObject
toDatum row =
  let (node:obj:x:y:dats) = words row in
  let datum = unwords.drop 2 $ dats in

  case obj of
  "canvas" -> Canvas node obj (read x, read y) coords2 datum
    where coords2 = (read $ dats!!0, read $ dats!!1)
  "connect" -> Connect node obj (read x) (read y) d0 d1
    where (d0, d1) = (read $ dats!!0, read $ dats!!1)
  "floatatom" -> FloatAtom node obj (read x, read y) len datum
    where len = read $ dats!!1
  "restore" -> Restore node obj (read x, read y) (unwords dats)
  "obj" -> Object node obj (read x, read y) (unwords dats)
  "msg" -> MSG node obj (read x, read y) (read.head $ dats)
  _ -> Empty -- throws away unexpected data

main = do  
  rawData <- readFile "examplePD.pd"
  let pureData = (map (tail.init)).(splitOn "\n") $ rawData
  let pureRecords = map toDatum pureData
  print pureRecords
