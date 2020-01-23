build: ../tapl_base.ml ch7.ml ch7_sub.ml ch7_lex.mll ch7_parse.mly ch7_bridge.ml
	-mkdir _build
	cp ../tapl_base.ml _build
	cp ch7.ml ch7_sub.ml ch7_lex.mll ch7_parse.mly ch7_bridge.ml _build
	cd _build && ocamllex ch7_lex.mll
	cd _build && ocamlyacc ch7_parse.mly
	cd _build && ocamlc -o ch7 tapl_base.ml ch7_sub.ml ch7_parse.mli ch7_parse.ml ch7_lex.ml ch7_bridge.ml ch7.ml
	cp _build/ch7 ./
