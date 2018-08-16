module Main where
import RayTracer.PhotographicPlate (processPlate)
import RayTracer.ParallelTracer (parallelTrace)
import RayTracer.FileToVector (fileToAry)
import RayTracer.DataWriter (savePlate)
import System.Environment (getArgs)
import RayTracer.Constants (size)
import Data.List (intercalate)
import Text.Regex

{--
Todo:
* pass (filename, distance, deviation, seed)
--}

flags = map mkRegex ["-x ([0-9]+)", "-d ([0-9]+)", "-s ([0-9]+)"]

testFilename = "this.hs -d 0 -s 1 -x 12"

parseFlags filename | filename == [] = ["x","x","x"]
                    | otherwise = map getVals $ f filename
  where
    f str = zipWith matchRegexAll flags (repeat str)

    getVals v = case v of
      (Just (_,_,_,[x])) -> x
      Nothing -> "x"

getFiles n =
    case n of
        100 -> ("./Data/dataStratifiedArray3D_100",
                "./Data/dataEmptyAry_10000")
        250 -> ("./Data/dataStratifiedArray3D_250",
                "./Data/dataEmptyAry_62500")
        500 -> ("./Data/dataStratifiedArray3D_500",
                "./Data/dataEmptyAry_250000")
        700 -> ("./Data/dataStratifiedArray3D_700",
                "./Data/dataEmptyAry_490000")
        1000 -> ("./Data/dataStratifiedArray3D_1000",
                 "./Data/dataEmptyAry_1000000")
main = do
    args <- getArgs
    case args of
        (filename:flags) -> do --default
            let [x, d, s] = parseFlags $ intercalate " " flags
            let (dFile, eFile) = getFiles 100
            ary <- fileToAry filename
            emptyAry <- fileToAry eFile
            plateAry <- parallelTrace ary
            let processedPlate = processPlate plateAry emptyAry
            savePlate "tmp" processedPlate

      [] -> do -- test path
        let (dFile, eFile) = getFiles size -- 100
        ary <- fileToAry dFile
        emptyAry <- fileToAry eFile
        plateAry <- parallelTrace ary
        let processedPlate = processPlate plateAry emptyAry
        savePlate "tmp" processedPlate

        _ -> putStrLn "Wrong number of arguments"
