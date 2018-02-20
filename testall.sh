#negative test test1-paren.arc
test1=$(./toplevel.native test1-paren.arc)
if [ "$test1" = "OKAY" ]; then
	echo "1: NEGATIVE TEST FAILED"
	else
		echo "1: NEGATIVE TEST WORKED"
fi

#negative test test2-comma.arc
test2=$(./toplevel.native test2-comma.arc)

if [ "$test2" = "OKAY" ]; then
	echo "2: NEGATIVE TEST FAILED"
	else
		echo "2: NEGATIVE TEST WORKED"
fi

#positive test test3-if-else.arc
test3=$(./toplevel.native test3-if-else.arc)
if [ "$test3" = "OKAY" ]; then
	echo "3: POSITIVE TEST WORKED"
	else
		echo "POSITIVE TEST FAILED"
fi

#negative test test4-string2int.arc
test4=$(./toplevel.native test4-string2int.arc)

if [ "$test4" = "OKAY" ]; then
	echo "4: NEGATIVE TEST FAILED"
	else
		echo "4: NEGATIVE TEST WORKED"
fi

#negative test test5-return_statement.arc
test5=$(./toplevel.native test5-return_statement.arc)

if [ "$test5" = "OKAY" ]; then
	echo "5: NEGATIVE TEST FAILED"
	else
		echo "5: NEGATIVE TEST WORKED"
fi

#positive test test6-positive_Stringconcat.arc
test6=$(./toplevel.native test6-positive_Stringconcat.arc)
if [ "$test6" = "OKAY" ]; then
	echo "6: POSITIVE TEST WORKED"
	else
		echo "6: POSITIVE TEST FAILED"
fi

#positive test test7-intdivision.arc
test7=$(./toplevel.native test7-intdivision.arc)
if [ "$test7" = "OKAY" ]; then
	echo "7: POSITIVE TEST WORKED"
	else
		echo "7: POSITIVE TEST FAILED"
fi