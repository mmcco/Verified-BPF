{
    open Lexing
    open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}


let digit = ['0'-'9']
let num = ['1'-'9'] digit*
let letter = ['a'-'z']
let opcode = letter (letter | '_')*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule bpf_lex =
  parse
  (* A <- P[k:4] *)
  | "ld_word" { OFFSET_OP (Lexing.lexeme lexbuf) }
  (* A <- P[k:2] *)
  | "ld_half" { OFFSET_OP (Lexing.lexeme lexbuf) }
  (* A <- P[k:1] *)
  | "ld_byte" { OFFSET_OP (Lexing.lexeme lexbuf) }
  (* A <- P[X+k:4] *)
  | "ld_ofst_word" { OFFSET_OP (Lexing.lexeme lexbuf) }
  (* A <- P[X+k:2] *)
  | "ld_ofst_half" { OFFSET_OP (Lexing.lexeme lexbuf) }
  (* A <- P[X+k:1] *)
  | "ldx_byte" { OFFSET_OP (Lexing.lexeme lexbuf) }
  (* A <- len *)
  | "ld_len" { LEN_OP (Lexing.lexeme lexbuf) }
  (* A <- k *)
  | "ld_imm" { IMM_OP (Lexing.lexeme lexbuf) }
  (* A <- M[k] *)
  | "ld_mem" { OFFSET_OP (Lexing.lexeme lexbuf) }
  
  (*
   Loads to index register
  *)
  (* X <- k *)
  | "ldx_imm" { IMM_OP (Lexing.lexeme lexbuf) }
  (* X <- M[k] *)
  | "ldx_mem" { OFFSET_OP (Lexing.lexeme lexbuf) }
  (* X <- len *)
  | "ldx_len" { LEN_OP (Lexing.lexeme lexbuf) }
  (* X <- hdr_len *)
  (* doesn't fit well, needs to be special case *)
  (* so, we keep it as a lone opcode, exclude HDR_LEN *)
  (* was: X <- 4*(P[k:1]&0xf) *)
  | "ldx_hdr_len" { OFFSET_OP (Lexing.lexeme lexbuf) }
  
  (* M[k] <- A *)
  | "store" { OFFSET_OP (Lexing.lexeme lexbuf) }
  
  (* M[k] <- X *)
  | "store_x" { OFFSET_OP (Lexing.lexeme lexbuf) }
  
  (* A <- A + k *)
  | "add_imm" { IMM_OP (Lexing.lexeme lexbuf) }
  (* A <- A - k *)
  | "sub_imm" { IMM_OP (Lexing.lexeme lexbuf) }
  (* A <- A * k *)
  | "mul_imm" { IMM_OP (Lexing.lexeme lexbuf) }
  (* A <- A / k *)
  | "div_imm" { IMM_OP (Lexing.lexeme lexbuf) }
  (* A <- A & k *)
  | "and_imm" { IMM_OP (Lexing.lexeme lexbuf) }
  (* A <- A | k *)
  | "or_imm" { IMM_OP (Lexing.lexeme lexbuf) }
  (* A <- A << k *)
  | "sl_imm" { IMM_OP (Lexing.lexeme lexbuf) }
  (* A <- A >> k *)
  | "sr_imm" { IMM_OP (Lexing.lexeme lexbuf) }
  (* A <- A + X *)
  | "add_x" { IMM_OP (Lexing.lexeme lexbuf) }
  (* A <- A - X *)
  | "sub_x" { IMM_OP (Lexing.lexeme lexbuf) }
  (* A <- A * X *)
  | "mul_x" { IMM_OP (Lexing.lexeme lexbuf) }
  (* A <- A / X *)
  | "div_x" { IMM_OP (Lexing.lexeme lexbuf) }
  (* A <- A & X *)
  | "and_x" { IMM_OP (Lexing.lexeme lexbuf) }
  (* A <- A | X *)
  | "or_x" { IMM_OP (Lexing.lexeme lexbuf) }
  (* A <- A << X *)
  | "sl_x" { IMM_OP (Lexing.lexeme lexbuf) }
  (* A <- A >> X *)
  | "sr_x" { IMM_OP (Lexing.lexeme lexbuf) }
  (* A <- -A *)
  | "neg" { IMM_OP (Lexing.lexeme lexbuf) }
  
  (* pc += k *)
  | "jmp_imm" { IMM_OP (Lexing.lexeme lexbuf) }
  (* pc += (A > k) ? jt : jf *)
  | "jgt_imm" { IMM_BR_OP (Lexing.lexeme lexbuf) }
  (* pc += (A >= k) ? jt : jf *)
  | "jge_imm" { IMM_BR_OP (Lexing.lexeme lexbuf) }
  (* pc += (A == k) ? jt : jf *)
  | "jeq_imm" { IMM_BR_OP (Lexing.lexeme lexbuf) }
  (* pc += (A & k) ? jt : jf *)
  | "jand_imm" { IMM_BR_OP (Lexing.lexeme lexbuf) }
  (* pc += (A > X) ? jt : jf *)
  | "jgt_x" { BR_OP (Lexing.lexeme lexbuf) }
  (* pc += (A >= X) ? jt : jf *)
  | "jge_x" { BR_OP (Lexing.lexeme lexbuf) }
  (* pc += (A == X) ? jt : jf *)
  | "jeq_x" { BR_OP (Lexing.lexeme lexbuf) }
  (* pc += (A & X) ? jt : jf *)
  | "jand_x" { BR_OP (Lexing.lexeme lexbuf) }
  
  (* Return, accept A bytes. *)
  | "ret_a" { SOLO_OP (Lexing.lexeme lexbuf) }
  (* Return, accept k bytes. *)
  | "ret_k" { SOLO_OP (Lexing.lexeme lexbuf) }
  
  (* X <- A *)
  | "x_store_a" { SOLO_OP (Lexing.lexeme lexbuf) }
  (* A <- X *)
  | "a_store_x" { SOLO_OP (Lexing.lexeme lexbuf) }

  (* syntactic sugar *)
  | "<-"              { bpf_lex lexbuf }
  (* syntactic sugar *)
  | "len"             { bpf_lex lexbuf }
  | num               { IMM (int_of_string (Lexing.lexeme lexbuf)) }
  | "P[" num "]"      { PKT_ADDR (int_of_string (Lexing.lexeme lexbuf)) }
  | "P[" num ":1]"    { PKT_ADDR (int_of_string (Lexing.lexeme lexbuf)) }
  | "P[" num ":2]"    { PKT_ADDR (int_of_string (Lexing.lexeme lexbuf)) }
  | "P[" (num as n) ":4]"    { print_endline n; PKT_ADDR (int_of_string n) }
  | "P[X+" num ":1]"  { PKT_ADDR (int_of_string (Lexing.lexeme lexbuf)) }
  | "P[X+" num ":2]"  { PKT_ADDR (int_of_string (Lexing.lexeme lexbuf)) }
  | "P[X+" num ":4]"  { PKT_ADDR (int_of_string (Lexing.lexeme lexbuf)) }
  | "M[" num "]"      { MEM_ADDR (int_of_string (Lexing.lexeme lexbuf)) }
  | "M[" num ":1]"    { MEM_ADDR (int_of_string (Lexing.lexeme lexbuf)) }
  | "M[" num ":2]"    { MEM_ADDR (int_of_string (Lexing.lexeme lexbuf)) }
  | "M[" num ":4]"    { MEM_ADDR (int_of_string (Lexing.lexeme lexbuf)) }
  | "M[X+" num ":1]"  { MEM_ADDR (int_of_string (Lexing.lexeme lexbuf)) }
  | "M[X+" num ":2]"  { MEM_ADDR (int_of_string (Lexing.lexeme lexbuf)) }
  | "M[X+" num ":4]"  { MEM_ADDR (int_of_string (Lexing.lexeme lexbuf)) }
  | white             { bpf_lex lexbuf }
  | newline           { next_line lexbuf; NEWLINE }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof               { EOF }


(*
{  
  let main () =
    let cin =
      if Array.length Sys.argv > 1
      then open_in Sys.argv.(1)
      else stdin
    in
    let lexbuf = Lexing.from_channel cin in
    bpf_lex lexbuf

  let _ = Printexc.print main ()
}
*)
