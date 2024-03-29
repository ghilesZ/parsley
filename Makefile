# Frontend to dune.

.PHONY: default build install uninstall test clean

default: build doc

build:
	dune build

doc:
	dune build @doc
	mkdir -p docs/
	cp -r _build/default/_doc/_html/parsley/* docs/

test:
	dune runtest -f

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean
