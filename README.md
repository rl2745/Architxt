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