#!/bin/bash
echo -n `./toplevel.native helloworld.arc > helloworld.ll`
echo -n `llc helloworld.ll > helloworld.s`
echo -n `cc -o helloworld.exe helloworld.s`
echo `./helloworld.exe`
files="helloworld.ll helloworld.s helloworld.exe"
rm -f $files
