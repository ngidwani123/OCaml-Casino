.PHONY: test check

test:
	OCAMLRUNPARAM=b dune exec test/test.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

doc:
	dune build @doc

opendoc:
	doc @bash opendoc.sh

zip:
	rm -f 3110-project.zip
	zip -r 3110-project.zip . 