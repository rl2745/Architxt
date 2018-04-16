# Architxt
Richard Lopez (rl2745@columbia.edu) - System Architect
Mihir Gulati (mg3540@columbia.edu) - Tester
Jenny Ni (jn2512@barnard.edu) - Manager
Tristan Orlofski (tio2001@columbia.edu) - Language Guru

This has been tested on and works on LLVM 3.8 

- to compile, run $make and this should give you toplevel.native
- to execute the program, run $./toplevel.native ____.arc where ____ is an .arc file. this will produce an llvm output
- to test the program, run ./testall2.sh
		it should output FAILED if the result doesn't match up with what was expected.

For semant.ml, there's still some issues with array return types in the matching and we still need to implement map and point in codegen.ml.
