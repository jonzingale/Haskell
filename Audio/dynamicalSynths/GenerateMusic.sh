#!/bin/bash

# This script compiles, cleans up, and plays music generated from RNA
# . GenerateMusic.sh

# Note: Main has two possible functions main.
# The first creates monophonic files of tracks
# The second produces a composition

echo 'compiling Main'
ghc -O2 --make Main.hs
echo 'removing files'
rm *.o *.hi
echo 'generating audio in longtones.wav'
time Main