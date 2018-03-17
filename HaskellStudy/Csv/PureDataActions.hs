module PureDataActions where
import PureDataParser hiding (main)
import Data.List.Split
import Data.List

main = do  
  rawData <- readFile "examplePD.pd"
  let pureData = (map (tail.init)).(splitOn "\n") $ rawData
  let pureRecords = map toDatum pureData
  let program = pureObjectsToProgram pureRecords
  let (tar:src:xs) = getObjects $ program
  print $ connect (src, 1, tar, 1, program)

-- This ought to be a tree to preserve indices and structure.
data PureDataProgram = Program { getNodes :: [PureDataObject],
                                 getFloatAtoms :: [PureDataObject],
                                 getConnections :: [PureDataObject],
                                 getMessages :: [PureDataObject],
                                 getObjects :: [PureDataObject]} deriving Show

pureObjectsToProgram :: [PureDataObject] -> PureDataProgram
pureObjectsToProgram xs =
  let canvas = filter ((== "canvas").objType) xs
      floatAtoms = filter ((== "floatAtom").objType) xs
      connections = filter ((== "connect").objType) xs
      messages = filter ((== "msg").objType) xs
      objects = filter ((== "obj").objType) xs in -- include restore
  Program canvas floatAtoms connections messages objects 

 -- Between a canvas and a restore.
-- getSubmodules :: PureDataProgram -> [PureDataProgram]

class Connection c where -- should probably append to some tmp, -> IO()
  connect :: (c, Int, c, Int, PureDataProgram) -> c

-- rewrite for trees will be much cleaner, for instance
-- MSG and the like should be connectable. Further, some
-- protections from self-connections, no inlet on inlet, etc...
instance Connection PureDataObject where
  connect (srcObj, outlet, tarObj, inlet, prg) =
    let (Just src, Just trg) = (dex srcObj prg , dex tarObj prg) in
    Connect "X" "connect" src outlet trg inlet
    where
      dex obj program = elemIndex obj (getObjects program)