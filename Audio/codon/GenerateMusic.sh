#!/bin/bash

# This script compiles, cleans up, and plays music generated from RNA
# . GenerateMusic.sh

ghc -O2 --make Main.hs
rm *.o *.hi
Main