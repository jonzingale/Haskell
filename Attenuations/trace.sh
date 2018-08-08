#!/bin/bash

# . trace.sh filename, if arg is blank tests for size found in Constants.hs
# file="$1"

# ghc -O2 MainWriter.hs
# time ./MainWriter
# rm MainWriter.o MainWriter.hi MainWriter
# echo 'finished writing necessary data'

echo 'starting tracer compilation'

# -prof -fprof-auto -fprof-cafs -fforce-recomp
# ghc -O2 --make Main.hs -threaded -rtsopts -fforce-recomp
ghc -O2 --make Main.hs -threaded -rtsopts -fforce-recomp # -prof -fprof-auto -fprof-cafs

clear ; echo 'starting trace' ; date

time ./Main +RTS -N8 # -sstderr -p # 8 virtual cores
rm Main.o Main.hi Main RayTracer/*.o RayTracer/*.hi

echo 'visualizing data'
python ./Visualizer/visualizer.py
rm Data/dataSavedPlate
