module Main where
import RayTracer.PhotographicPlate (processPlate)
import RayTracer.ParallelTracer (parallelTrace)
import RayTracer.FileToVector (fileToAry)
import RayTracer.DataWriter (savePlate)
import System.Environment (getArgs)
import RayTracer.Constants (size)
import Data.List (intercalate)
import Text.Regex

testData = ("./Data/dataStratifiedArray3D_100", "./Data/dataEmptyAry_10000")
bigData  = ("./Data/dataStratifiedArray3D_1000", "./Data/dataEmptyAry_1000000")
flags = map mkRegex ["-x ([0-9]+)", "-d ([0-9]+)", "-s ([0-9]+)"]

parseFlags filename
    | filename == [] = ["","",""]
    | otherwise = map getVals $ f filename
  where
    f str = zipWith matchRegexAll flags (repeat str)
    getVals v = case v of
      (Just (_,_,_,[x])) -> x
      Nothing -> ""

getFiles n =
    case n of
        100 -> testData
        1000 -> bigData

distanceIs x = do
    if x == ""
    then putStr "\ndefault distance: 2 * 10^3"
    else putStr ("\ndistance is set to: "++x)

deviationIs d = do
    if d == ""
    then putStr "\ndefault deviation: 2"
    else putStr ("\ndeviation is set to: "++d)

seedIs s = do
    if s == ""
    then putStr "\ndefault seed: 23"
    else putStr ("\nseed is set to: "++s)

main = do
    args <- getArgs
    case args of
        (filename:flags) -> do -- file path
            let [x, d, s] = parseFlags $ intercalate " " flags
            putStr ("Loading File: " ++ filename)
            distanceIs x
            deviationIs d
            seedIs s
            let (dFile, eFile) = getFiles 100
            ary <- fileToAry filename
            emptyAry <- fileToAry eFile
            plateAry <- parallelTrace ary
            let processedPlate = processPlate plateAry emptyAry
            savePlate "tmp" processedPlate

        [] -> do -- test path
            putStr "Test Path"
            let (dFile, eFile) = getFiles size -- 100
            ary <- fileToAry dFile
            emptyAry <- fileToAry eFile
            plateAry <- parallelTrace ary
            let processedPlate = processPlate plateAry emptyAry
            savePlate "tmp" processedPlate