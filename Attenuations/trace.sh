#!/bin/bash

# file="$1"
# preset for 8 threads
# time for benchmarking
# time ./Main $file +RTS -N8


ghc -O2 --make Main.hs -threaded -rtsopts
time ./Main +RTS -N8
rm Main.o Main.hi Main RayTracer/*.o RayTracer/*.hi
python ./Visualizer/visualizer.py