Require Parser.
Require Datatypes.
Require Import ExtrOcamlBasic.
Require Import ExtrOcamlString.

Extraction Language Ocaml.

Extraction "Main.ml" Main.Make.
Extraction "Parser.ml" Parser.pinstrs Datatypes.get_token.
