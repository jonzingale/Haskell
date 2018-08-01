module Main where
import RayTracer.PhotographicPlate (processPlate)
import RayTracer.ParallelTracer (parallelTrace)
import RayTracer.FileToVector (fileToAry)
import RayTracer.DataWriter (savePlate)
import System.Environment (getArgs)
import RayTracer.Constants (size)

{--
Todo:
* pass (filename, seed) to compiled tracer
* produce file of comparable size.
* scale constants

To Compile and Run:
ghc -O2 --make Main.hs -threaded -rtsopts
time ./Main +RTS -N8
time ./Main Tests/data1M +RTS -N8

Additional CPU data:
time ./Main +RTS -s -N8

To Clear:
rm Main.o Main.hi Main RayTracer/*.o RayTracer/*.hi
--}

getFiles n =
    case n of
        100 -> ("./Tests/dataStratifiedArray3D_100",
                "./Tests/dataEmptyAry_10000")
        250 -> ("./Tests/dataStratifiedArray3D_250",
                "./Tests/dataEmptyAry_62500")
        500 -> ("./Tests/dataStratifiedArray3D_500",
                "./Tests/dataEmptyAry_250000")
        1000 -> ("./Tests/dataBigSparsey",
                 "./Tests/dataBigSparsey")
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
