SERVER_MODULES= db
OBJECTS=$(MODULES:=.cmo)
OCAMLBUILD=ocamlbuild -use-ocamlfind
server_name = server

build-server:
	$(OCAMLBUILD) $(OBJECTS)
	dune build server.exe --profile debug

# Build and run the app
run-server: build-server
	dune exec ./$(server_name).exe

# Build and run the app with Opium's internal debug messages visible
run-debug:
	dune exec ./$(server_name).exe -- --debug
