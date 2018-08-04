#!/bin/bash

file="$1"

# Writing Zeros is important the first time.
# ghc -O2 MainWriter.hs
# time ./MainWriter 100
# rm MainWriter.o MainWriter.hi MainWriter
# echo 'finished data writing'

echo 'starting tracer compilation'
ghc -O2 --make Main.hs -threaded -rtsopts
echo 'starting trace'
time ./Main +RTS -N8 # 8 virtual cores
# time ./Main +RTS $file -N8 # 8 virtual cores, pass file to script.
rm Main.o Main.hi Main RayTracer/*.o RayTracer/*.hi

echo 'visualizing data'
python ./Visualizer/visualizer.py
