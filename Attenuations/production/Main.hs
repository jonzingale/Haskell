module Main where
import qualified Data.Vector.Unboxed as U
import RayTracer.PhotographicPlate (processPlate)
import RayTracer.ParallelTracer (parallelTrace)
import RayTracer.FileToVector (fileToAry)
import System.Environment (getArgs)
import RayTracer.Constants (size)
import Data.List (intercalate)
import Text.Regex

type Lattice = U.Vector Double

regexes = repeat "([0-9]+\\.?[0-9]*)"
flags = map mkRegex $ zipWith (++) ["-x ", "-d ", "-s "] regexes
testTraceString = "Test Trace\ndistance: 1000mm\ndeviation: 2\nseed: 23\n"

sparseData = "./Data/sparseArray3D"
emptyData  = "./Data/emptyArray2D"

parseArgs :: String -> String -> String -> String -> IO((Double, Double, Int))
parseArgs filename x d s = do
  let distance  = if x == "" then "2000" else x
  let deviation = if d == "" then "2" else d
  let seed = if s == "" then "23" else s
  putStr $ "Loading File: " ++ filename ++
           "\ndistance: " ++ distance ++ "mm" ++
           "\ndeviation: "++deviation ++
           "\nseed: " ++ seed ++ "\n"
  return $ ((read distance)::Double,
            (read deviation)::Double,
            (read seed)::Int)

parseFlags :: String -> [String]
parseFlags filename
    | filename == [] = ["","",""]
    | otherwise = map getVals $ f filename
  where
    f str = zipWith matchRegexAll flags (repeat str)
    getVals v = case v of
      Just (_,_,_,[x]) -> x
      Nothing -> ""

savePlate :: Lattice -> IO()
savePlate ary =
  writeFile "./Data/savedPlate" $ aryToStr ary
  where aryToStr = unlines.(map show).(U.toList)

main = do
    args <- getArgs
    emptyAry <- fileToAry emptyData

    case args of
        (filename:flags) -> do
            let [x, d, s] = parseFlags $ intercalate " " flags
            (distance, deviation, seed) <- parseArgs filename x d s
            ary <- fileToAry filename
            plateAry <- parallelTrace ary distance deviation seed
            let processedPlate = processPlate plateAry emptyAry
            savePlate processedPlate

        [] -> do
            putStr testTraceString
            ary <- fileToAry sparseData
            plateAry <- parallelTrace ary 1000 2 23 -- distance deviation seed
            let processedPlate = processPlate plateAry emptyAry
            savePlate processedPlate
