#!/bin/bash

# file="$1 $2 $3 $4 $5 $6 $7"

ghc -O2 Main.hs

rm Main.o Main.hi Line.hi Line.o
