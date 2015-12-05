#!/bin/bash

grep ".*[aeiou].*[aeiou].*[aeiou].*" day5input > regex1output
wc -l regex1output
grep -E "(.)\1{1}" regex1output > regex2output
wc -l regex2output
grep -Ev "ab|cd|pq|xy" regex2output > regex3output
wc -l regex3output

rm regex*output
