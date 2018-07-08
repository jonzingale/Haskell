#!/bin/bash

file="$1"
# preset for 8 threads
time ./Main $file +RTS -N8