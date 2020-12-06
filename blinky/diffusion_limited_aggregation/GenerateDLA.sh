#!/bin/bash

# This script compiles, cleans up, and plays music generated from RNA
# . GenerateDLA.sh

# parallel
# echo 'compiling parallel Main'
# ghc -O2 --make Main.hs -threaded -rtsopts -fforce-recomp
# echo 'generating diffusion limited aggregate'
# time ./Main +RTS -N6
# echo 'removing files'
# rm *.o *.hi

# accelerate
# echo 'compiling parallel Main'
# ghc -O2 --make Main.hs
# echo 'generating diffusion limited aggregate'
# time ./Main +RTS -A64M -n2M -RTS
# echo 'removing files'
# rm *.o *.hi

# classic
echo 'compiling classic Main'
ghc -O2 --make Main.hs
echo 'generating diffusion limited aggregate'
time ./Main
echo 'removing files'
rm *.o *.hi

# profiler
# https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html
# echo 'compiling profiler Main'
# ghc -O2 --make Main.hs -prof -fprof-auto -fprof-cafs -rtsopts
# echo 'removing files'
# rm *.o *.hi
# echo 'generating diffusion limited aggregate'
# time ./Main +RTS -p