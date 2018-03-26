# Architxt
Richard Lopez (rl2745@columbia.edu) - System Architect
Mihir Gulati (mg3540@columbia.edu) - Tester
Jenny Ni (jn2512@barnard.edu) - Manager
Tristan Orlofski (tio2001@columbia.edu) - Language Guru

This has been tested on and works on LLVM 3.8 

- to compile, run $make and this should give you toplevel.native
	* note that the ast.ml pretty printing prints "OKAY" at the end of an accepted file for testing purposes
	* note that while ocamlbuild does not throw any warnings, if one compiles the .mly file by itself, it may return warnings that x token is unused. This is expected because we may plan on using these tokens in the future.
- to execute the program, run $./toplevel.native ____.arc where ____ is an .arc file. this will produce an llvm output
- to test the helloworld program, run $./hellotest.sh
	it should output hello world if it works.

For semant.ml, there's still some issues with array return types in the matching and we still need to implement map and point in codegen.ml.