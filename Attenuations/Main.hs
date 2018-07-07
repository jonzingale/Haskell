module Main where
import RayTracer.FileToVector
import RayTracer.Transport
import RayTracer.Crossings
import RayTracer.ParallelTracer
import System.Random

{--
Todo:
* Simulate 1M rays from gaussian point source

To Run:
ghc -O2 --make Main.hs -threaded -rtsopts
time ./Main +RTS -N4

To Clear:
rm Main.o Main.hi Main
--}

main = do parallelTrace
