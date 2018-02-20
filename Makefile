
toplevel.native: ast.ml toplevel.ml parser.mly scanner.mll
	ocamlbuild toplevel.native


.PHONY:clean
clean: 
	ocamlbuild -clean


