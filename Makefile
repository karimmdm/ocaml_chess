MODULES= game interactive state piece gui logic authors client
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

SERVER_MODULES= server/db server/server
SERVER_MLS=$(SERVER_MODULES:=.ml)
SERVER_MLIS=$(SERVER_MODULES:=.mli)
SERVER_OBJECTS=$(SERVER_MODULES:=.cmo)
server_name = server

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

play:
	$(OCAMLBUILD) -tag 'debug' $(MAIN) && OCAMLRUNPARAM=b ./$(MAIN)

zip:
	zip chess_engine.zip *mli *.md *.ml* *.json *.sh _tags .merlin .ocamlformat .ocamlinit Makefile images/ server/ -r 
	
docs: docs-public docs-private
	
docs-public: build
	mkdir -p _doc.public
	ocamlfind ocamldoc -I _build -package graphics,yojson,ANSITerminal,curly,camlimages.png,camlimages.graphics,opium,ppx_deriving_yojson\
		-html -stars -d _doc.public $(MLIS)

docs-private: build
	mkdir -p _doc.private
	ocamlfind ocamldoc -I _build -package graphics,yojson,ANSITerminal,curly,camlimages.png,camlimages.graphics,opium,ppx_deriving_yojson\
		-html -stars -d _doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS) 

clean:
	ocamlbuild -clean
	rm -rf _doc.public _doc.private chess.zip

