MODULES=frame state piece gui logic printer authors
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
FRAME=frame.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

play:
	$(OCAMLBUILD) -tag 'debug' $(MAIN) && OCAMLRUNPARAM=b ./$(MAIN)

frame: 
	$(OCAMLBUILD) -tag 'debug' $(FRAME) && OCAMLRUNPARAM=b ./$(FRAME)

check:
	@bash check.sh
	
finalcheck:
	@bash check.sh final

zip:
	zip chess_engine.zip *.md *.ml* *.json *.sh _tags .merlin .ocamlformat .ocamlinit Makefile images/ -r 
	
docs: docs-public docs-private
	
docs-public: build
	mkdir -p _doc.public
	ocamlfind ocamldoc -I _build -package graphics, camlimages.png, camlimages.graphics\
		-html -stars -d _doc.public $(MLIS)

docs-private: build
	mkdir -p _doc.private
	ocamlfind ocamldoc -I _build -package graphics, camlimages.png, camlimages.graphics\
		-html -stars -d _doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf _doc.public _doc.private chess.zip
