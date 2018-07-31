
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Vector.Unboxed as U

test = "./Tests/dataAllOnes100"

fileToAry :: FilePath -> IO (U.Vector Double)
fileToAry file = do
  s <- L.readFile file
  return $ U.fromList $ readDouble s

readDouble :: L.ByteString -> [Double]
readDouble bs = map (read.(L.unpack)) $ L.lines $ bs
  