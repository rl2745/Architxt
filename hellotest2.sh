#!/bin/bash
echo -n `./toplevel.native helloworld.arc > helloworld.ll`
echo -n `llc helloworld.ll > helloworld.s`
echo -n `cc -o helloworld.exe helloworld.s`
echo -n `./helloworld.exe > helloworld.txt`
sequence="`cat helloworld.txt`"
pattern="hello world"
case "$sequence" in
	"$pattern" ) echo "OK";;
	*) echo "FAIL";;
esac
rm helloworld.txt helloworld.ll helloworld.s helloworld.exe

