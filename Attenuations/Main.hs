module Main where
import RayTracer.FileToVector (fileToAry)
import RayTracer.GaussianNozzle
import RayTracer.ParallelTracer
import RayTracer.PhotographicPlateFloat
import System.Environment
{--
Todo:
* Simulate 1M rays from gaussian point source
* pass (filename, seed) to compiled tracer
* thread and return fileType.
* self-contained bash script.
* produce file of comparable size.

To Compile and Run:
ghc -O2 --make Main.hs -threaded -rtsopts
time ./Main +RTS -N8
time ./Main Tests/data1M +RTS -N8

To Clear:
rm Main.o Main.hi Main RayTracer/*.o RayTracer/*.hi
--}

testFile = fileToAry "./Tests/data1M"

main = do
    args <- getArgs
    case args of
      [file] -> do
        ary <- fileToAry file
        parallelTrace ary
      [] -> do
        ary <- testFile
        parallelTrace ary
      _ -> putStrLn "Wrong number of arguments"
