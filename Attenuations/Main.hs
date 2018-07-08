module Main where
import RayTracer.GaussianNozzle
import RayTracer.ParallelTracer
import RayTracer.PhotoPlate

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

To Clear:
rm Main.o Main.hi Main RayTracer/*.o RayTracer/*.hi
--}

main = do parallelTrace
