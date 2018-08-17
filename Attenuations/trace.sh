#!/bin/bash

# . trace.sh <FILENAME>
# . trace.sh ./Data/dataStratifiedArray3D_1000
file="$1 $2 $3 $4 $5 $6 $7"
# f1="$2"
# f2="$3"
# f3="$4"
# f4="$5 $6"
# deviation="$3"
# seed="$4"

# ghc -O2 MainWriter.hs
# time ./MainWriter
# cp MainWriter ./production/DataWriter
# rm MainWriter.o MainWriter.hi MainWriter
# echo 'finished writing necessary data'

# echo 'starting tracer compilation'
# -prof -fprof-auto -fprof-cafs -fforce-recomp
ghc -O2 --make Main.hs -threaded -rtsopts -fforce-recomp # -prof -fprof-auto -fprof-cafs
# ghc -O2 --make InterfaceTest.hs

# echo "starting trace: $(date)"
time ./Main $file +RTS -N8 # -sstderr -p # 8 virtual cores
# time ./InterfaceTest $file
# cp Main ./production/
rm Main.o Main.hi Main RayTracer/*.o RayTracer/*.hi
# rm InterfaceTest.o InterfaceTest.hi InterfaceTest 

echo 'visualizing data'
python ./Visualizer/visualizer.py
rm Data/dataSavedPlate

# . production/raytracer.sh ./Data/dataStratifiedArray3D_1000
