module Main where
import RayTracer.DataWriter (savePlate, parseArgs)
import RayTracer.PhotographicPlate (processPlate)
import RayTracer.ParallelTracer (parallelTrace)
import RayTracer.FileToVector (fileToAry)
import System.Environment (getArgs)
import RayTracer.Constants (size)
import Data.List (intercalate)
import Text.Regex

regexes = repeat "([0-9]+\\.?[0-9]*)"
flags = map mkRegex $ zipWith (++) ["-x ", "-d ", "-s "] regexes
testTraceString = "Test Trace\ndistance: 1000mm\ndeviation: 2\nseed: 23\n"
testData = ("./Data/dataStratifiedArray3D_100" , "./Data/dataEmptyAry_10000")
bigData  = ("./Data/dataStratifiedArray3D_1000", "./Data/dataEmptyAry_1000000")

-- Todo: -t flag for 100 test otherwise always 1000

parseFlags filename
    | filename == [] = ["","",""]
    | otherwise = map getVals $ f filename
  where
    f str = zipWith matchRegexAll flags (repeat str)
    getVals v = case v of
      Just (_,_,_,[x]) -> x
      Nothing -> ""

getFiles n =
    case n of
        100 -> testData
        1000 -> bigData

main = do
    args <- getArgs
    case args of
        (filename:flags) -> do
            let [x, d, s] = parseFlags $ intercalate " " flags
            putStr ("Loading File: " ++ filename)
            (distance, deviation, seed) <- parseArgs x d s

            let (dFile, eFile) = getFiles 100
            ary <- fileToAry filename
            emptyAry <- fileToAry eFile
            plateAry <- parallelTrace ary distance deviation seed
            let processedPlate = processPlate plateAry emptyAry
            savePlate "tmp" processedPlate

        [] -> do
            putStr testTraceString
            let (dFile, eFile) = getFiles size -- 100
            ary <- fileToAry dFile
            emptyAry <- fileToAry eFile
            plateAry <- parallelTrace ary 1000 2 23 -- distance deviation seed
            let processedPlate = processPlate plateAry emptyAry
            savePlate "tmp" processedPlate