#!/bin/bash

# . trace.sh <FILENAME>
# . trace.sh ./Data/dataStratifiedArray3D_1000
file="$1"

ghc -O2 MainWriter.hs
# time ./MainWriter
cp MainWriter ./production/DataWriter
rm MainWriter.o MainWriter.hi MainWriter
echo 'finished writing necessary data'

echo 'starting tracer compilation'
# -prof -fprof-auto -fprof-cafs -fforce-recomp
ghc -O2 --make Main.hs -threaded -rtsopts -fforce-recomp # -prof -fprof-auto -fprof-cafs

echo "starting trace: $(date)"
# time ./Main $file +RTS -N8 # -sstderr -p # 8 virtual cores
cp Main ./production/
rm Main.o Main.hi Main RayTracer/*.o RayTracer/*.hi

# echo 'visualizing data'
# python ./Visualizer/visualizer.py
# rm Data/dataSavedPlate

. production/trace.sh ./Data/dataStratifiedArray3D_1000
