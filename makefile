OPT=-classic-display -use-ocamlfind -plugin-tags package\(js_of_ocaml.ocamlbuild\)

all: 
	ocamlbuild $(OPT) ttcsim.native
	ocamlbuild $(OPT) ttcsim.byte
	ocamlbuild $(OPT) ttcasm.native
	ocamlbuild $(OPT) ttcasm.byte

web: 
	ocamlbuild $(OPT) ttcwebdemo.js

clean:
	ocamlbuild -clean
	rm *~
