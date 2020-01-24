test : test.ml test1.txt test2.txt test3.txt test4.txt test5.txt test6.txt
	-mkdir _build
	cp test.ml test1.txt test2.txt test3.txt test4.txt test5.txt test6.txt _build
	cd _build && ocamlc -o test test.ml
	cd _build && ./test