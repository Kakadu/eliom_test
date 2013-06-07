ocamlbuild := ocamlbuild -use-ocamlfind -classic-display -j 1
main_server := $(addprefix _server/, server.cma)
main_client := $(addprefix _client/, client.js)
all := all.otarget

all: server client

server:
	$(ocamlbuild) $(main_server)

client:
	$(ocamlbuild) $(main_client)

run:
	ocsigenserver -c ocsigen.conf -V

run.opt:
	ocsigenserver.opt -c ocsigen.conf

clean:
	$(ocamlbuild) -clean
