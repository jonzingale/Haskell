module Main where
import RayTracer.ParallelTracer

{--
Todo:
* Simulate 1M rays from gaussian point source
* pass (filename, seed) to compiled tracer
* thread and return fileType.
* Find optimal threading.

To Compile and Run:
ghc -O2 --make Main.hs -threaded -rtsopts
time ./Main +RTS -N8

To Clear:
rm Main.o Main.hi Main
--}

main = do parallelTrace
