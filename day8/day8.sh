#!/bin/bash

# First, encode the difference in characters in #-signs
# Then at the end remove everything that is not a #, split it in separate lines and count them
sed 's/\\\\/#/g; s/\\"/#/g; s/\\x[a-f0-9][a-f0-9]/###/g; s/\"/#/g; s/[^#]//g' day8input | grep -o \# | wc -l

sed 's/"/#/g; s/^/##/g; s/\\/#/g; s/\\/#/g; s/[^#]//g' day8input | grep -o \# | wc -l
