Require Main.
Require Parser.
Extraction Language Ocaml.

Extraction "main.ml" Main.Make.
Extraction "Datatypes.ml" Parser.pinstrs.
