open Array
open Sys
open Lexing
open Lexer
open Parser
open Parser.Inter

(* hackily ignoring negatives for testing's sake *)
let rec int_to_nat (i: int) : Analysis.nat =
    if i < 1 then O
    else S (int_to_nat (i-1)) 

let rec nat_to_int (n: Analysis.nat) : int =
    match n with
      | O -> 0
      | S n' -> 1 + nat_to_int n'

let word_to_int (sz: Analysis.nat) (w: Analysis.word) : int =
    nat_to_int (Analysis.wordToNat sz w)

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
                    print_endline ("runtime success! returned " ^ (string_of_int (word_to_int (int_to_nat 32) w)))
            | Error chars ->
                    print_endline ("runtime error: " ^ (BatString.of_list chars))))
