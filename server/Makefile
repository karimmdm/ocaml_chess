project_name = server

# The opam package configuration file
opam_file = $(project_name).opam


# Alis to update the opam file and install the needed deps
deps: $(opam_file)

build: 
	dune build server.exe --profile debug

# Build and run the app
run:
	dune exec ./$(project_name).exe

# Build and run the app with Opium's internal debug messages visible
run-debug:
	dune exec ./$(project_name).exe -- --debug
