{
    open Lexing
    open Datatypes.Gram
    (*open Parser*)

exception SyntaxError of string

let init filename channel : Lexing.lexbuf =

  push_context := begin fun () -> contexts := []::!contexts end;
  pop_context := begin fun () ->
    match !contexts with
      | [] -> assert false
      | t::q -> List.iter (Hashtbl.remove lexicon) t;
                contexts := q
  end;

 declare_varname := begin fun id ->
   if Hashtbl.mem lexicon id then begin
     Hashtbl.add lexicon id (fun loc -> VAR_NAME (id, ref VarId, loc));
     match !contexts with
       | [] -> ()
       | t::q -> contexts := (id::t)::q
     end
  end;

  declare_typename := begin fun id ->
    Hashtbl.add lexicon id (fun loc -> TYPEDEF_NAME (id, ref TypedefId, loc));
    match !contexts with
      | [] -> ()
      | t::q -> contexts := (id::t)::q
  end;

  !declare_typename "__builtin_va_list";

  let lb = Lexing.from_channel channel in
  lb.lex_curr_p <-
    {lb.lex_curr_p with pos_fname = filename; pos_lnum = 1};
  lb

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
  | solo_op { SOLO_OP't (Lexing.lexeme lexbuf) }
  | br_op { BR_OP't (Lexing.lexeme lexbuf) }
  | len_op { LEN_OP't (Lexing.lexeme lexbuf) }
  | imm_op { IMM_OP't (Lexing.lexeme lexbuf) }
  | imm_br_op { IMM_BR_OP't (Lexing.lexeme lexbuf) }
  | offset_op { OFFSET_OP't (Lexing.lexeme lexbuf) }

  (* syntactic sugar *)
  | "<-"  { bpf_lex lexbuf }
  | "len" { bpf_lex lexbuf }

  | num as n   { IMM (int_of_string n) }

  | "P[" (num as n) "]"                 { PKT_ADDR't (int_of_string n) }
  | "P[" (num as n) ':' val_size ']'    { PKT_ADDR't (int_of_string n) }
  | "P[X+" (num as n) ':' val_size ']'  { PKT_ADDR't (int_of_string n) }

  | "M[" (num as n) "]"                 { MEM_ADDR't (int_of_string n) }
  | "M[" (num as n) ':' val_size ']'    { MEM_ADDR't (int_of_string n) }
  | "M[X+" (num as n) ':' val_size ']'  { MEM_ADDR't (int_of_string n) }

  | white             { bpf_lex lexbuf }
  | newline           { next_line lexbuf; NEWLINE }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof               { EOF }

(*
{
  open Streams
  open Specif
  open Parser
  open Aut.GramDefs

  let tokens_stream lexbuf : token coq_Stream =
    let tokens = Queue.create () in
    let lexer_wraper lexbuf : Pre_parser.token =
      let res =
        if lexbuf.lex_curr_p.pos_cnum = lexbuf.lex_curr_p.pos_bol then
          initial_linebegin lexbuf
        else
          initial lexbuf
      in
      Queue.push res tokens;
      res
    in
    Pre_parser.translation_unit_file lexer_wraper lexbuf;
    assert (!contexts = []);
    let rec compute_token_stream () =
      let loop t v =
        Cons (Coq_existT (t, Obj.magic v), Lazy.from_fun compute_token_stream)
      in
      match Queue.pop tokens with
    in
    Lazy.from_fun compute_token_stream

}

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
