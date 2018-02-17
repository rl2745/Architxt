#!/bin/sh

./compile.sh $1
./$1
rm $1
