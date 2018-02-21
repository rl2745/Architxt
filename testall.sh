#negative test test1-paren.arc
test1=$(./toplevel.native tests/test1-paren.arc)
if [ "$(grep -c "OKAY" <<< $test1)" == 1 ]; then
	echo "1: NEGATIVE TEST FAILED"
	else
		echo "1: NEGATIVE TEST WORKED"
fi

#negative test test2-comma.arc
test2=$(./toplevel.native tests/test2-comma.arc)

if [ "$(grep -c "OKAY" <<< $test2)" == 1 ]; then
	echo "2: NEGATIVE TEST FAILED"
	else
		echo "2: NEGATIVE TEST WORKED"
fi

#positive test test3-if-else.arc
test3=$(./toplevel.native tests/test3-if-else.arc)
if [ "$(grep -c "OKAY" <<< $test3)" == 1 ]; then
	echo "3: POSITIVE TEST WORKED"
	else
		echo "POSITIVE TEST FAILED"
fi

#negative test test4-string2int.arc
test4=$(./toplevel.native tests/test4-string2int.arc)

if [ "$(grep -c "OKAY" <<< $test4)" == 1 ]; then
	echo "4: NEGATIVE TEST FAILED"
	else
		echo "4: NEGATIVE TEST WORKED"
fi

#negative test test5-return_statement.arc
test5=$(./toplevel.native tests/test5-return_statement.arc)
if [ "$(grep -c "OKAY" <<< $test5)" == 1 ]; then
	echo "5: NEGATIVE TEST FAILED"
	else
		echo "5: NEGATIVE TEST WORKED"
fi

#positive test test6-positive_Stringconcat.arc
test6=$(./toplevel.native tests/test6-positive_Stringconcat.arc)
if [ "$(grep -c "OKAY" <<< $test6)" == 1 ]; then
	echo "6: POSITIVE TEST WORKED"
	else
		echo "6: POSITIVE TEST FAILED"
fi

#positive test test7-intdivision.arc
test7=$(./toplevel.native tests/test7-intdivision.arc)
if [ "$(grep -c "OKAY" <<< $test7)" == 1 ]; then
	echo "7: POSITIVE TEST WORKED"
	else
		echo "7: POSITIVE TEST FAILED"
fi

#positive test test8-array.arc
test8=$(./toplevel.native tests/test8-array.arc)
if [ "$(grep -c "OKAY" <<< $test8)" == 1 ]; then
	echo "8: POSITIVE TEST WORKED"
	else
		echo "8: POSITIVE TEST FAILED"
fi

#positive test test9-point.arc
test9=$(./toplevel.native tests/test9-point.arc)
if [ "$(grep -c "OKAY" <<< $test9)" == 1 ]; then
	echo "9: POSITIVE TEST WORKED"
	else
		echo "9: POSITIVE TEST FAILED"
fi

#negative test test10-map.arc
test10=$(./toplevel.native tests/test10-map.arc)
if [ "$(grep -c "OKAY" <<< $test10)" == 1 ]; then
	echo "10: NEGATIVE TEST FAILED"
	else
		echo "10: NEGATIVE TEST WORKED"
fi
