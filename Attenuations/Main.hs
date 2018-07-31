module Main where
import RayTracer.PhotographicPlate (processPlate)
import RayTracer.ParallelTracer (parallelTrace)
import RayTracer.FileToVector (fileToAry)
import RayTracer.DataWriter (savePlate)
import System.Environment (getArgs)
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

To Change:
center @ GaussianBeam
size   @ ParallelTracer
size   @ PhotographicPlate
range(0, size**3) @ visualizer
--}

-- testFile = fileToAry "./Tests/dataStratifiedArray3D_100"
-- testFile = fileToAry "./Tests/dataStratifiedArray3D_250"
testFile = fileToAry "./Tests/dataStratifiedArray3D_500"
-- testFile = fileToAry "./Tests/dataBigSparsey"

main = do
    -- emptyAry <- fileToAry "./Tests/dataEmptyAry_10000" -- 100
    -- emptyAry <- fileToAry "./Tests/dataEmptyAry_62500" -- 250
    emptyAry <- fileToAry "./Tests/dataEmptyAry_250000" -- 500

    args <- getArgs
    case args of
      [file] -> do
        ary <- fileToAry file
        plateAry <- parallelTrace ary
        let processedPlate = processPlate plateAry emptyAry
        savePlate "tmp" processedPlate
      [] -> do
        ary <- testFile
        plateAry <- parallelTrace ary
        let processedPlate = processPlate plateAry emptyAry
        savePlate "tmp" processedPlate
      _ -> putStrLn "Wrong number of arguments"
