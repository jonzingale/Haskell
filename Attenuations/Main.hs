module Main where
import RayTracer.PhotographicPlate (processPlate)
import RayTracer.ParallelTracer (parallelTrace)
import RayTracer.FileToVector (fileToAry)
import RayTracer.DataWriter (savePlate)
import System.Environment (getArgs)
import RayTracer.Constants (size)

{--
Todo:
* pass (
  filename, seed, distance, deviation,
  couponCollected Beam) to compiled tracer

* verify params
--}

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

      [filename] -> do -- default path
        let (dFile, eFile) = getFiles 1000
        ary <- fileToAry filename
        emptyAry <- fileToAry eFile
        plateAry <- parallelTrace ary
        let processedPlate = processPlate plateAry emptyAry
        savePlate "tmp" processedPlate

      [filename, seed] -> do -- with random seed
        let (dFile, eFile) = getFiles 1000
        ary <- fileToAry filename
        emptyAry <- fileToAry eFile
        plateAry <- parallelTrace ary
        let processedPlate = processPlate plateAry emptyAry
        savePlate "tmp" processedPlate

      [] -> do -- test path
        let (dFile, eFile) = getFiles size --100
        ary <- fileToAry dFile
        emptyAry <- fileToAry eFile
        plateAry <- parallelTrace ary
        let processedPlate = processPlate plateAry emptyAry
        savePlate "tmp" processedPlate

      _ -> putStrLn "Wrong number of arguments"
