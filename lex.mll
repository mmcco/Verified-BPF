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
let val_size = '1' | '2' | '4'
let letter = ['a'-'z']
let opcode = letter (letter | '_')*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let offset_op =
  (* A <- P[k:4] *)
    "ld_word"
  (* A <- P[k:2] *)
  | "ld_half"
  (* A <- P[k:1] *)
  | "ld_byte"
  (* A <- P[X+k:4] *)
  | "ld_ofst_word"
  (* A <- P[X+k:2] *)
  | "ld_ofst_half"
  (* A <- P[X+k:1] *)
  | "ldx_byte"
  (* A <- M[k] *)
  | "ld_mem"
  (* X <- M[k] *)
  | "ldx_mem"
  (* M[k] <- A *)
  | "store"
  (* M[k] <- X *)
  | "store_x"

let solo_op =
  (* Return, accept A bytes. *)
    "ret_a"
  (* Return, accept k bytes. *)
  | "ret_k"
  (* X <- A *)
  | "x_store_a"
  (* A <- X *)
  | "a_store_x"
  (* X <- hdr_len *)
  (* doesn't fit well, needs to be special case *)
  (* so, we keep it as a lone opcode, exclude HDR_LEN *)
  (* was: X <- 4*(P[k:1]&0xf) *)
  | "ldx_hdr_len"

let imm_op =
  (* A <- k *)
    "ld_imm"
  (* A <- A + k *)
  | "add_imm"
  (* A <- A - k *)
  | "sub_imm"
  (* A <- A * k *)
  | "mul_imm"
  (* A <- A / k *)
  | "div_imm"
  (* A <- A & k *)
  | "and_imm"
  (* A <- A | k *)
  | "or_imm"
  (* A <- A << k *)
  | "sl_imm"
  (* A <- A >> k *)
  | "sr_imm"
  (* A <- A + X *)
  | "add_x"
  (* A <- A - X *)
  | "sub_x"
  (* A <- A * X *)
  | "mul_x"
  (* A <- A / X *)
  | "div_x"
  (* A <- A & X *)
  | "and_x"
  (* A <- A | X *)
  | "or_x"
  (* A <- A << X *)
  | "sl_x"
  (* A <- A >> X *)
  | "sr_x"
  (* A <- -A *)
  | "neg"
  (* pc += k *)
  | "jmp_imm"
  (* X <- k *)
  | "ldx_imm"

let imm_br_op =
  (* pc += (A > k) ? jt : jf *)
    "jgt_imm"
  (* pc += (A >= k) ? jt : jf *)
  | "jge_imm"
  (* pc += (A == k) ? jt : jf *)
  | "jeq_imm"
  (* pc += (A & k) ? jt : jf *)
  | "jand_imm"
  
let len_op =
  (* A <- len *)
    "ld_len"
  (* X <- len *)
  | "ldx_len"

let br_op =
  (* pc += (A > X) ? jt : jf *)
    "jgt_x"
  (* pc += (A >= X) ? jt : jf *)
  | "jge_x"
  (* pc += (A == X) ? jt : jf *)
  | "jeq_x"
  (* pc += (A & X) ? jt : jf *)
  | "jand_x"


rule bpf_lex =
  parse
  | solo_op { SOLO_OP (Lexing.lexeme lexbuf) }
  | br_op { BR_OP (Lexing.lexeme lexbuf) }
  | len_op { LEN_OP (Lexing.lexeme lexbuf) }
  | imm_op { IMM_OP (Lexing.lexeme lexbuf) }
  | imm_br_op { IMM_BR_OP (Lexing.lexeme lexbuf) }
  | offset_op { OFFSET_OP (Lexing.lexeme lexbuf) }

  (* syntactic sugar *)
  | "<-"  { bpf_lex lexbuf }
  | "len" { bpf_lex lexbuf }

  | num as n   { IMM (int_of_string n) }

  | "P[" (num as n) "]"                 { PKT_ADDR (int_of_string n) }
  | "P[" (num as n) ':' val_size ']'    { PKT_ADDR (int_of_string n) }
  | "P[X+" (num as n) ':' val_size ']'  { PKT_ADDR (int_of_string n) }

  | "M[" (num as n) "]"                 { MEM_ADDR (int_of_string n) }
  | "M[" (num as n) ':' val_size ']'    { MEM_ADDR (int_of_string n) }
  | "M[X+" (num as n) ':' val_size ']'  { MEM_ADDR (int_of_string n) }

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
