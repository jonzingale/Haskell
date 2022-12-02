#!/bin/bash

# This script compiles, cleans up, and executes code
# . script.sh

# execute
# echo 'compiling Main'
# ghc-9.2.5  -O2 -o Main VectorRSA.hs
# echo 'removing files'
# rm *.o *.hi
# echo 'encrypting; decrypting; printing'
# time Main
# rm Main

# profiler
# https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html
echo 'compiling profiler Main'
ghc -O2 -o --make Main Main.hs -prof -fprof-auto -fprof-cafs -rtsopts
echo 'removing files'
rm *.o *.hi
echo 'running Main'
time ./Main +RTS -p
rm Main