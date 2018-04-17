# Architxt
Richard Lopez (rl2745@columbia.edu) - System Architect
Mihir Gulati (mg3540@columbia.edu) - Tester
Jenny Ni (jn2512@barnard.edu) - Manager
Tristan Orlofski (tio2001@columbia.edu) - Language Guru

This has been tested on and works on LLVM 3.8 

- to compile, run $make and this should give you toplevel.native
- to execute a program, run $./toplevel.native ____.arc where ____ is an .arc file. this will produce an llvm output
- to test the program, run ./testall.sh
		it should output the test name and OK or FAILED with a message depending on the results. it will produce a testall.log with commands it executed and where it failed.

Points have been added, though array and maps need to be added. It should not throw any warnings when using $make command.

test-array
test-bool
test-float
test-hello = tests if it can print out hello world
test-ifelse
test-intdiv
test-point2 = tests assignment of point using variables instead of literals
test-pointaccess2 = tests access of a point's surface
test-pointaccess = tests access of a point's name
test-point = tests the basic assignment of a point
test-pointassign2 = tests assignment of point's surface after creation
test-pointassign3 = tests assignment of point values after creation with IDs
test-pointassign = tests assignment of a point's name after creation
test-printi = tests the print_i feature which prints integers
test-string = tests creating and assigning a string
fail-comma
fail-map
fail-paren
fail-pointaccess2 = tests invalid assignment of string to int in point access
fail-pointaccess = tests attempting to print out boolean and bad keyword Surface
fail-pointassign2 = tests that it rejects assignment of non-strings to name
fail-pointassign = tests bad keyword Name
fail-pointtyp = tests that it rejects invalid types when creating points
fail-return_statement
fail-stringconcat
fail-stringint