#!/bin/bash

# usage:
# . compile.sh ; time ./Main "audio/peptideSymphony.wav" 1

# file="$1 $2"

ghc -O2 Main.hs

rm Main.o Main.hi Line.hi Line.o

