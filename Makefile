# Frontend to dune.

.PHONY: default build install uninstall test clean

default: build

build:
	dune build

doc:
	dune build @doc

test:
	dune runtest -f

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean
