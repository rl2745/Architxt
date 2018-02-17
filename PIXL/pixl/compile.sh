#!/bin/sh

./pixl.native < $1.p > $1.ll
llc $1.ll
gcc -o $1 $1.s stdlib.o
rm $1.ll $1.s
