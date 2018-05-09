#!/bin/bash
echo -n `./toplevel.native test-array2.arc > test-array2.ll`
echo -n `llc test-array2.ll > test-array2.s`
echo -n `cc -o test-array2.exe test-array2.s`
echo `./test-array2.exe`
rm -f test-array2.ll test-array2.s test-array2.exe

