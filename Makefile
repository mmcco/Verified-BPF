VAL_FILES=validator/Alphabet.v \
	validator/Tuples.v \
	validator/Grammar.v \
	validator/Automaton.v \
	validator/Validator_safe.v \
	validator/Validator_complete.v \
	validator/Interpreter.v \
	validator/Interpreter_correct.v \
	validator/Interpreter_complete.v \
	validator/Interpreter_safe.v \
	validator/Main.v

BPF_FILES=Bpf.v Parser.v

all:
	ocamlc -c bpf.ml
	#menhir --infer -v parser.mly
	menhir --coq -v Parser.vy
	#ocamlc -c parser.mli
	#ocamlc -c parser.ml
	ocamllex Lexer.mll
	#ocamlc -c Lexer.ml
	#coqc -I validator validator/Alphabet.v validator/Tuples.v \
		validator/Grammar.v validator/Automaton.v validator/Main.v
	
	coqc -I validator -I . ${VAL_FILES} ${BPF_FILES}
	coqc -I validator -I . extraction.v
	ocamlc main.mli main.ml
	ocamlc Datatypes.mli Datatypes.ml
	ocamlc -I validator -I . Parse.ml

clean:
	rm -f a.out *.o *.vo *.cmo *.cmx *.cmi *.mli *.automaton \
		*.glob Lexer.ml Parser.ml Parser.v validator/*.vo \
		validator/*.glob
