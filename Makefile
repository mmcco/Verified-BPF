all:
	set -e

	ocamlc -c bpf.ml
	menhir --infer -v parser.mly
	ocamlc -c parser.mli
	ocamlc -c parser.ml
	ocamllex lex.mll
	ocamlc -c lex.ml
	ocamlopt lex.ml parser.ml bpf.ml main.ml

clean:
	rm *.cmo *.cmi *.mli *.automaton lex.ml parser.ml
