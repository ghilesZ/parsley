CC   = ocamlfind ocamlc
EXE  = test
PP   = preprocess
LIBS = -linkpkg -package compiler-libs.common -package zarith
PPX  = -ppx "./$(PP) -as-ppx"

all: exe

bootstrap: ppx
	$(CC) parsley.ml ppx_parsley.ml -linkpkg -package $(LIBS) -o $(PP) $(PPX)

test: exe
	./$(EXE)

exe: ppx
	$(CC) test.ml $(PPX) -o $(EXE)

ppx:
	$(CC) utils.ml parsley.ml ppx_parsley.ml $(LIBS) -o $(PP)

clean:
	rm -f `find . -name "*.o"`
	rm -f `find . -name "*.a"`
	rm -f `find . -name "*.cm*"`
	rm -f `find . -name "*~"`
	rm -f `find . -name "\#*"`
	rm -f $(PP) $(EXE)
