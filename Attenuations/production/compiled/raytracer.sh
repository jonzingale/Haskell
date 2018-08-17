#!/bin/bash

# may need PIL installed via pip

# . raytracer.sh <FILENAME> -x <Distance mm> -d <STANDARD DEVIATION> -s <SEED>
# . raytracer.sh ./Data/sparseArray3D -x 1000 -d 2 -s 23
file="$1 $2 $3 $4 $5 $6 $7"

time ./DataWriter ; echo ''
echo 'finished writing necessary data'
echo "starting trace: $(date)"
time ./Main $file +RTS -N8 # 8 virtual cores
echo 'visualizing data'
python ./visualizer.py $file
# rm Data/dataSavedPlate
