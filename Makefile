ch4: ch4.ml ch4_lex.mll ch4_parse.mly
#	cp ch4_sub.ml ch4_lex.mll ch4_parse.mly ch4.ml ch4_build
	ocamllex ch4_lex.mll
	ocamlyacc ch4_parse.mly
	ocamlc -o ch4 ch4_sub.ml ch4_parse.mli ch4_parse.ml ch4_lex.ml ch4.ml
	./ch4


clean:
	@rm -rf *.cmi *.cmx *.cmo *.o ch4 *.out ch4_lex.ml ch4_parse.ml ch4_parse.mli ch4_build
