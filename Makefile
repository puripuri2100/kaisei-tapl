ch4: ch4.ml
	ocamlfind ocamlopt -o ch4 -linkpkg ch4.ml
	./ch4

clean:
	rm -rf *.cmi *.cmx *.o ch4
