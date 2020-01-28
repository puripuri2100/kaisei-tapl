build: ch4.ml ch4_lex.mll ch4_parse.mly ch4_base.ml
	-mkdir _build
	cp *.ml *.mll *.mly _build
	cd _build && ocamllex ch4_lex.mll
	cd _build && ocamlyacc ch4_parse.mly
	cd _build && ocamlc -o ch4 ch4_base.ml ch4_sub.ml ch4_parse.mli ch4_parse.ml ch4_lex.ml ch4.ml
	cp _build/ch4 ./