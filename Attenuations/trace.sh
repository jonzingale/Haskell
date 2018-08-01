#!/bin/bash

# file="$1"
# preset for 8 threads
# time for benchmarking
# time ./Main $file +RTS -N8


# ghc -O2 --make Main.hs -threaded -rtsopts
echo 'started at:'
date +%H-%M-%S
time ./Main +RTS -N8
echo 'finished trace at:'
date +%H-%M-%S
# rm Main.o Main.hi Main RayTracer/*.o RayTracer/*.hi
# echo 'removed files, visualizing data'
python ./Visualizer/visualizer.py
