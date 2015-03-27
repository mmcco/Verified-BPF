open Array
open Sys
open Lexing
open Lexer
open Parser
open Parser.Inter

let _ =
    let rec inf = S inf in

    let cin =
      if Array.length Sys.argv > 1
      then open_in Sys.argv.(1)
      else stdin
    in
    let lexbuf = Lexing.from_channel cin in
    (match pinstrs inf (Lexer.tokens_stream lexbuf) with
      | Fail_pr ->
              print_endline "parser failed!"
      | Timeout_pr ->
              print_endline "parser timed out!"
      | Parsed_pr (output, _) ->
          let ins : Analysis.instr list = Obj.magic output in
          (match Analysis.prog_eval ins Analysis.init_state (S O) with
            | Ret w ->
                    print_endline "runtime success!"
            | Error chars ->
                    print_endline (BatString.of_list chars)))
