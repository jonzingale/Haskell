#!/bin/bash

# . trace.sh filename, if arg is blank tests for size found in Constants.hs
file="$1"

ghc -O2 MainWriter.hs
time ./MainWriter
rm MainWriter.o MainWriter.hi MainWriter
echo 'finished writing necessary data'

echo 'starting tracer compilation'
ghc -O2 --make Main.hs -threaded -rtsopts
echo 'starting trace'
time ./Main +RTS $file -N8 # 8 virtual cores
rm Main.o Main.hi Main RayTracer/*.o RayTracer/*.hi

echo 'visualizing data'
python ./Visualizer/visualizer.py
