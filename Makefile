CC   = ocamlfind ocamlc
EXE  = test
PP   = preprocess
LIBS = compiler-libs.common

all: exe

test: exe
	./$(EXE)

exe: ppx
	$(CC) test.ml -ppx "./$(PP) -as-ppx" -o $(EXE)

ppx:
	$(CC) check.ml parsley.ml -linkpkg -package $(LIBS) -o $(PP)

clean:
	rm -f `find . -name "*.o"`
	rm -f `find . -name "*.a"`
	rm -f `find . -name "*.cm*"`
	rm -f `find . -name "*~"`
	rm -f `find . -name "\#*"`
	rm -f $(PP) $(EXE)
