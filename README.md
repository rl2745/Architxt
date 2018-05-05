# Architxt
Richard Lopez (rl2745@columbia.edu) - System Architect
Mihir Gulati (mg3540@columbia.edu) - Tester
Tristan Orlofski (tio2001@columbia.edu) - Language Guru

This has been tested on and works on LLVM 3.8

Compliation Instructions:
    - run $make and this creates toplevel.native
    - to execute a program, run ./toplevel.native____.arc where ____ is an .arc file. this will produce an llvm output
    - to teset the program, run ./testall.sh
        this should output the testname and OK or FAILED with a message (depending upon the results). It will also produce a testall.log with commands that it executed and where exactly it failed.

TO-DO:

Points have been added, though array and maps need to be added. It should not throw any warnings when using $make command.

test-array - tests array assignment, intialization, and setting an array index 
  to a value
test-bool = tests initialization and assignment of a boolean
test-float = tests initialization and assignment of a float
test-hello = tests if it can print out hello world
test-ifelse = tests if-else statements in Architxt by testing if an int is 
  greater than 0
test-intdiv = tests whether an initialized integer value can be divided
test-point2 = tests assignment of point using variables instead of literals
test-pointaccess2 = tests access of a point's surface
test-pointaccess = tests access of a point's name
test-point = tests the basic assignment of a point
test-pointassign2 = tests assignment of point's surface after creation
test-pointassign3 = tests assignment of point values after creation with IDs
test-pointassign = tests assignment of a point's name after creation
test-printi = tests the print_i feature which prints integers
test-string = tests creating and assigning a string
fail-comma = tests invalid ending of a statement with a comma rather than a 
  semicolon
fail-map = tests incorrect declaration of a map using a ) instead of a ] 
fail-paren = tests incorrect use of brackets instead of parenthesis to enclose 
  the right side of an assignment
fail-pointaccess2 = tests invalid assignment of string to int in point access
fail-pointaccess = tests attempting to print out boolean and bad keyword Surface
fail-pointassign2 = tests that it rejects assignment of non-strings to name
fail-pointassign = tests bad keyword Name
fail-pointtyp = tests that it rejects invalid types when creating points
fail-return_statement = tests incorrect return statement not ended with a ";"
fail-stringconcat = tries to concatenate a string, which is not allowed in 
  Architxt
fail-stringint = tests whether strings can be treated as ints by adding a 
  string variable to an integer
