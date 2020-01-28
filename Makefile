.PHONY: ch4 ch7 ch10 clean

ch4: ch4
	cd ch4 && make build


ch7: ch7
	cd ch7 && make build


ch10: ch10
	cd ch10 && make build


ch10_test: ch10
	cd ch10/test && make test


clean:
	@rm -rf */*.cmi */*.cmx */*.cmo */*.o */*.out */_build */ch4 */ch7 */ch10
