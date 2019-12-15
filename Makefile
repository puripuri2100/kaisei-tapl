ch4: tapl_base.ml ch4/ch4.ml ch4/ch4_lex.mll ch4/ch4_parse.mly
	cd ch4 && make build


ch4_test : ch4
	ch4/ch4 ch4/ch4_test/test1.txt


ch7: tapl_base.ml ch7/ch7.ml ch7/ch7_sub.ml ch7/ch7_lex.mll ch7/ch7_parse.mly
	cd ch7 && make build


.PHONY: clean

clean:
	@rm -rf */*.cmi */*.cmx */*.cmo */*.o */ch4 */*.out */_build */ch7
