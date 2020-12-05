#!/bin/bash

# This script compiles, cleans up, and plays music generated from RNA
# . GenerateDLA.sh

echo 'compiling Main'
ghc -O2 --make Main.hs
echo 'removing files'
rm *.o *.hi
echo 'generating diffusion limited aggregate'
Main