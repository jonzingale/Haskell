#!/bin/bash

# This script compiles, cleans up, and plays music generated from RNA
# . GenerateDLA.sh

# parallel
# echo 'compiling Main'
# ghc -O2 --make Main.hs -threaded -rtsopts -fforce-recomp
# echo 'removing files'
# rm *.o *.hi
# echo 'generating diffusion limited aggregate'
# time ./Main +RTS -N8

# classic
echo 'compiling Main'
ghc -O2 --make Main.hs
echo 'removing files'
rm *.o *.hi
echo 'generating diffusion limited aggregate'
time ./Main