.PHONY: ch4 ch7 ch10 clean

ch4: tapl_base.ml ch4/ch4.ml ch4/ch4_lex.mll ch4/ch4_parse.mly
	cd ch4 && make build


ch4_test : ch4
	ch4/ch4 ch4/ch4_test/test1.txt


ch7: ch7
	cd ch7 && make build


ch10: ch10
	cd ch10 && make build


clean:
	@rm -rf */*.cmi */*.cmx */*.cmo */*.o */*.out */_build */ch4 */ch7 */ch10
