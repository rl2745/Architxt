
.PHONY: toplevel.native
toplevel.native: 
	rm -f *.o
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis -cflags -w,+a-4 \
		toplevel.native
	

	

.PHONY:clean
clean: 
	ocamlbuild -clean


