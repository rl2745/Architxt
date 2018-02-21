# Architxt
Richard Lopez (rl2745@columbia.edu) - System Architect
Mihir Gulati (mg3540@columbia.edu) - Tester
Jenny Ni (jn2512@barnard.edu) - Manager
Tristan Orlofski (tio2001@columbia.edu) - Language Guru

- to compile, run $make and this should give you toplevel.native
	* note that due to the way our testing script was written, it will return a warning that ast was unused because we did not print out the ast and printed out OKAY instead if the file is valid
- to execute the program, run $./toplevel.native ____.arc where ____ is an .arc file
- to test, run $./testall.sh
	all of the tests should return #: POSITIVE/NEGATIVE TEST WORKED with # as the test number

We still need to add in more robust array features to implement our map and expand on extra point features.
