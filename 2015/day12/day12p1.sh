#!/bin/bash

grep -o "\-\?[0-9]*" day12input | awk '{ sum += $1 } END { print sum }'
