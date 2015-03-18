open Lexing

module CharSet = Set.Make(struct type t = char let compare = compare end)
(*
  let main () =
    let cin =
      if Array.length Sys.argv > 1
      then open_in Sys.argv.(1)
      else stdin
    in
    let lexbuf = Lexing.from_channel cin in
    bpf_lex lexbuf

  let _ = Printexc.print main ()
  *)

let () =
  let cin =
    if Array.length Sys.argv > 1
    then open_in Sys.argv.(1)
    else stdin
  in
  let lexbuf = Lexing.from_channel cin in
  let rec inf = Datatypes.S inf in
  (* let pinstrs = Datatypes.pinstrs Lex.bpf_lex lexbuf in *)
  let p =
    Obj.magic
      (match Datatypes.pinstrs inf lexbuf with
         | Datatypes.Parser.Inter.Fail_pr ->
             (* Theoretically impossible : implies inconsistencies
                between grammars. *)
                 print_endline "Internal error while parsing"
                 None
         | Datatypes.Parser.Inter.Timeout_pr ->
                 print_endline "Timeout while parsing"
                 None
         | Datatypes.Parser.Inter.Parsed_pr (ast, _ ) ->
                 ast)
  in
  p
