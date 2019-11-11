ch4: ch4.ml
	-mkdir ch4_build
	cd ch4_build
	ocamlfind ocamlopt -o ch4 -linkpkg ch4.ml
	./ch4


clean:
	@rm -rf *.cmi *.cmx *.o ch4