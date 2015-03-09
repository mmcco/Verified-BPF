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
  let lb = Lexer.init name ic in
  let p =
    Obj.magic
      (match Parser.pinstrs inf (Lexer.tokens_stream lb) with
         | Parser.Parser.Inter.Fail_pr ->
             (* Theoretically impossible : implies inconsistencies
                between grammars. *)
                 print_endline "Internal error while parsing"
                 None
         | Parser.Parser.Inter.Timeout_pr ->
                 print_endline "Timeout while parsing"
                 None
         | Parser.Parser.Inter.Parsed_pr (ast, _ ) ->
                 ast)
  in
  p
