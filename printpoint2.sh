#!/bin/bash
echo -n `./toplevel.native test-pointaccess2.arc > test-pointaccess.ll`
echo -n `llc test-pointaccess.ll > test-pointaccess.s`
echo -n `cc -o test-pointaccess.exe test-pointaccess.s`
echo `./test-pointaccess.exe`
rm -f test-pointaccess.ll test-pointaccess.s test-pointaccess.exe

