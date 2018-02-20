#!/bin/sh
#script to run testing in shell

#negative test test1-paren.arc
test1=$(./toplevel.native test1-paren.arc)

if [ "$test1" = "OKAY" ]; then
	echo "NEGATIVE TEST FAILED"
	else
		echo "NEGATIVE TEST OKAY"
fi

#negative test test2-comma.arc

test2=$(./toplevel.native test2-comma.arc)

if [ "$test2" = "OKAY" ]; then
	echo "NEGATIVE TEST FAILED"
	else
		echo "NEGATIVE TEST OKAY"
fi

#positive test test3-if-else.arc

test3=$(./toplevel.native test3-if-else.arc)
if [ "$test3" = "OKAY" ]; then
	echo "POSITIVE TEST OKAY"
	else
		echo "POSITIVE TEST FAILED"
fi

#negative test test4-string2int.arc
test4=$(./toplevel.native test4-string2int.arc)

if [ "$test4" = "OKAY" ]; then
	echo "NEGATIVE TEST FAILED"
	else
		echo "NEGATIVE TEST OKAY"
fi

#negative test test5-return_statement.arc
test5=$(./toplevel.native test5-return_statement.arc)

if [ "$test5" = "OKAY" ]; then
	echo "NEGATIVE TEST FAILED"
	else
		echo "NEGATIVE TEST OKAY"
fi

#positive test test6-positive_Stringconcat.arc
test6=$(./toplevel.native test6-positive_Stringconcat.arc)
if [ "$test6" = "OKAY" ]; then
	echo "POSITIVE TEST OKAY"
	else
		echo "POSITIVE TEST FAILED"
fi

#positive test test7-intdivision.arc
test7=$(./toplevel.native test7-intdivision.arc)
if [ "$test7" = "OKAY" ]; then
	echo "POSITIVE TEST OKAY"
	else
		echo "POSITIVE TEST FAILED"
fi