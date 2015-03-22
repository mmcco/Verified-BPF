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

COQ_DIRS=-I . -I validator -I includes -I bedrock

all:
	# necessary Bedrock libraries
	coqc bedrock/Labels.v
	coqc bedrock/Nomega.v
	coqc bedrock/Reflection.v
	coqc bedrock/EqdepClass.v
	coqc bedrock/PropX.v
	coqc -I bedrock bedrock/NatMap.v
	coqc -I bedrock bedrock/Word.v
	coqc -I bedrock bedrock/Memory.v
	coqc -I bedrock bedrock/Decidables.v
	coqc -I bedrock bedrock/DepList.v
	coqc -I bedrock bedrock/Heaps.v
	coqc -I bedrock bedrock/PropXTac.v
	coqc -I bedrock bedrock/IL.v
	coqc -I bedrock bedrock/SepTheoryX.v
	coqc -I bedrock bedrock/SepTheoryXIL.v
	coqc -I bedrock bedrock/SepIL.v
	# the CompCert parser validator
	coqc -I validator ${VAL_FILES}
	# includes from other parts of CompCert
	ocamlc -c includes/Specif.mli
	ocamlc -I includes -c includes/Specif.ml
	ocamlc -c includes/Streams.mli
	ocamlc -I includes -c includes/Streams.ml
	${MAKE} incr

incr:
	ocamllex Lexer.mll
	menhir --coq Parser.vy
	coqc ${COQ_DIRS} Parser.v
	coqc ${COQ_DIRS} Datatypes.v
	coqc ${COQ_DIRS} Analysis.v
	coqc ${COQ_DIRS} Extract.v
	#ocamlc -c Word.mli
	#ocamlc -c Word.ml
	ocamlc -c Parser.mli
	ocamlfind ocamlc -package batteries -I includes -c Lexer.ml
	ocamlc -c Parser.ml
	ocamlc -c Main.mli
	ocamlc -c Main.ml
	ocamlfind ocamlc -package batteries -linkpkg \
		Parser.cmo Lexer.cmo run.ml

clean:
	rm -f *.glob *.cmi *.cmo a.out *.vo Parser.ml Lexer.ml \
		Parser.v validator/*.{glob,vo} Main.ml \
		Main.mli Parser.mli Lexer.mli Word.ml \
		includes/*.cm{o,i} bedrock/*.{glob,vo} \
		Analysis.ml{,i}
