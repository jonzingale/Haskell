#!/bin/bash

ghc -O2 --make WaveTest.hs
time ./WaveTest
rm *.hi *.o WaveTest
open temp.wav