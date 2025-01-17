MODULES=state compare pot main table deck authors probability betting mainfuncs
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=ounit2,str

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS)

playable:
	$(OCAMLBUILD) $(MAIN)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

play:
	$(OCAMLBUILD) -tag 'debug' $(MAIN) && OCAMLRUNPARAM=b ./$(MAIN)

zip:
	zip poker.zip poker.sh *.ml* *.json _tags .merlin .ocamlformat .ocamlinit LICENSE Makefile	
	
docs: docs-public
	
docs-public: build
	mkdir -p _doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d _doc.public $(MLIS)

clean:
	ocamlbuild -clean
	rm -rf _doc.public _doc.private poker.zip
