build: ch10.ml ch10_sub.ml ch10_bridge.ml ch10_typechecking.ml ch10_base.ml ch10_lex.mll ch10_parse.mly
	-mkdir _build
	cp ch10_sub.ml ch10_base.ml ch10_bridge.ml ch10_typechecking.ml ch10.ml ch10_lex.mll ch10_parse.mly _build
	cd _build && ocamllex ch10_lex.mll
	cd _build && ocamlyacc ch10_parse.mly
	cd _build && ocamlc  ch10_sub.ml ch10_base.ml ch10_bridge.ml ch10_typechecking.ml ch10_parse.mli ch10_parse.ml ch10_lex.ml ch10.ml -o ch10

clean:
	rm -rf _build