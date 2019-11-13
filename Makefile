ch4: ch4_build/ch4.cmx ch4_build/ch4_parser.cmx
	ocamlopt -o ch4_build/ch4 ch4_build/ch4_parser.cmx ch4_build/ch4.cmx
	./ch4_build/ch4


ch4_build/ch4.cmx : ch4.ml
	ocamlopt -o ch4_build/ch4.cmx -c ch4.ml

ch4_build/ch4_parser.cmx : ch4_parser.ml
	ocamlopt -o ch4_build/ch4_parser.cmx -c ch4_parser.ml

clean:
	@rm -rf ./ch4_build/*