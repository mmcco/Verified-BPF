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

COQ_DIRS=-I . -I validator -I includes -I bedrock/src

all:
	# independent Bedrock libraries
	#coqc bedrock/Labels.v
	#coqc bedrock/Nomega.v
	#coqc bedrock/Reflection.v
	#coqc bedrock/EqdepClass.v
	# this one takes longer than the rest combined
	#coqc bedrock/PropX.v
	${MAKE} dependent

dependent:
	#coqc -I bedrock bedrock/Folds.v
	#coqc -I bedrock bedrock/NatMap.v
	#coqc -I bedrock bedrock/Word.v
	#coqc -I bedrock bedrock/Memory.v
	#coqc -I bedrock bedrock/Decidables.v
	#coqc -I bedrock bedrock/DepList.v
	#coqc -I bedrock bedrock/Expr.v
	#coqc -I bedrock bedrock/Heaps.v
	#coqc -I bedrock bedrock/PropXTac.v
	#coqc -I bedrock bedrock/PropXRel.v
	#coqc -I bedrock bedrock/IL.v
	#coqc -I bedrock bedrock/SepTheoryX.v
	#coqc -I bedrock bedrock/SepTheoryXIL.v
	#coqc -I bedrock bedrock/SepExpr.v
	#coqc -I bedrock bedrock/SepIL.v
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
	coqc Skipn.v
	coqc ${COQ_DIRS} -R bedrock/src/ Bedrock Parser.v
	coqc ${COQ_DIRS} -R bedrock/src/ Bedrock Datatypes.v
	coqc ${COQ_DIRS} -R bedrock/src/ Bedrock Analysis.v
	coqc ${COQ_DIRS} -R bedrock/src/ Bedrock Extract.v
	ocamlc -c Parser.mli
	ocamlfind ocamlc -package batteries -I includes -c Lexer.ml
	ocamlc -c Parser.ml
	ocamlc -c Main.mli
	ocamlc -c Main.ml
	ocamlc -c Analysis.mli
	ocamlc -c Analysis.ml
	ocamlfind ocamlc -package batteries -linkpkg \
		Analysis.cmo Parser.cmo Lexer.cmo run.ml

clean:
	rm -f *.glob *.cmi *.cmo a.out *.vo Parser.ml Lexer.ml \
		Parser.v validator/*.{glob,vo} Main.ml \
		Main.mli Parser.mli Lexer.mli Word.ml \
		includes/*.cm{o,i} \
		Analysis.ml{,i}
