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
  filename, size, seed, distance, deviation,
  couponCollected Beam) to compiled tracer

* produce file of comparable size.
* scale constants

To Compile and Run:
ghc -O2 --make Main.hs -threaded -rtsopts
time ./Main +RTS -N8
time ./Main Data/dataStratifiedArray3D_100 +RTS -N8

To Clear:
rm Main.o Main.hi Main RayTracer/*.o RayTracer/*.hi
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
    let (dFile, eFile) = getFiles size -- remove once finished
    emptyAry <- fileToAry eFile
    args <- getArgs
    case args of
      -- [file] -> do
      --   ary <- fileToAry file
      --   plateAry <- parallelTrace ary
      --   let processedPlate = processPlate plateAry emptyAry
      --   savePlate "tmp" processedPlate
      [] -> do
        ary <- fileToAry dFile
        plateAry <- parallelTrace ary
        let processedPlate = processPlate plateAry emptyAry
        savePlate "tmp" processedPlate
      _ -> putStrLn "Wrong number of arguments"
