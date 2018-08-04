#!/bin/bash

ghc -O2 --make WaveTest.hs -threaded -rtsopts
time ./WaveTest +RTS -N8
rm *.hi *.o WaveTest