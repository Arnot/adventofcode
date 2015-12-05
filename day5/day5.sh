#!/bin/bash

echo "Task 1"
grep ".*[aeiou].*[aeiou].*[aeiou].*" day5input | grep -E "(.)\1" | grep -Ev "ab|cd|pq|xy" | wc -l

echo "Task 2"
grep -E "(..).*\1" day5input | grep -E "(.).\1" | wc -l

