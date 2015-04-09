Require Parser.
Require Datatypes.
Require Analysis.
Require Word.

Require Import ExtrOcamlBasic.
Require Import ExtrOcamlString.

Extraction Language Ocaml.

Extraction "Main.ml" Main.Make.
Extraction "Parser.ml" Parser.pinstrs Datatypes.get_token.
Extraction "Analysis.ml" Analysis.state Analysis.run_filter Word.wordToNat Word.natToWord.
