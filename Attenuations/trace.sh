#!/bin/bash

file="$1"
# preset for 8 threads
# time for benchmarking
time ./Main $file +RTS -N8