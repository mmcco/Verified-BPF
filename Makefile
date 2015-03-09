all:
	ocamlc -c bpf.ml
	#menhir --infer -v parser.mly
	menhir --coq -v parser.vy
	#ocamlc -c parser.mli
	#ocamlc -c parser.ml
	ocamllex lex.mll
	#ocamlc -c lex.ml
	coqc -I validator validator/Alphabet.v validator/Tuples.v \
		validator/Grammar.v validator/Automaton.v #validator/Main.v
	
	#coqc validator/Grammar.v
	#coqc validator/Automaton.v
	#coqc validator/Tuples.v
	#coqc validator/Main.v
	coqc -I validator parser.v ext.v
	ocamlopt lex.ml bpf.ml main.ml

clean:
	rm -f a.out *.o *.cmo *.cmx *.cmi *.mli *.automaton \
		*.glob lex.ml parser.ml parser.v validator/*.vo
