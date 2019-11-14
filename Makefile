ch4: ch4.ml ch4_lex.mll ch4_parse.mly tapl_base.ml
	-mkdir ch4_build
	cp tapl_base.ml ch4_sub.ml ch4_lex.mll ch4_parse.mly ch4.ml ch4_build
	cd ch4_build && ocamllex ch4_lex.mll
	cd ch4_build && ocamlyacc ch4_parse.mly
	cd ch4_build && ocamlc -o ch4 tapl_base.ml ch4_sub.ml ch4_parse.mli ch4_parse.ml ch4_lex.ml ch4.ml
	cp ch4_build/ch4 ./

ch4_test : ch4
	./ch4 ch4_test/test1.txt

.PHONY: clean

clean:
	@rm -rf *.cmi *.cmx *.cmo *.o ch4 *.out ch4_lex.ml ch4_parse.ml ch4_parse.mli ch4_build
