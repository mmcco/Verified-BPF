{
    open Lexing
    open Printf
    open Specif
    open Parser
    (*open Main*)
    open Aut.GramDefs
    (*open Streams*)
    open BatString
    open String

    exception SyntaxError of string

    let rec of_int (n: int) : imm =
        assert (n >= 0);
        if n = 0 then O else S (of_int (pred n))

    let to_num n =
        of_int (int_of_string n)

}

let num = ['0' - '9']+
let whitespace = [' ' '\t' '\012' '\r']
let newline = '\n'
let op = "+" | "-" | "*" | "/" | "**"
let val_size = '1' | '2' | '4'


rule lex = parse
  | "P[" (num as n) "]"
    { get_token (PKT_ADDR'tok (to_num n)) }
  | "P[" (num as n) ':' val_size ']'
    { get_token (PKT_ADDR'tok (to_num n)) }
  | "P[X+" (num as n) ':' val_size ']'
    { get_token (PKT_ADDR'tok (to_num n)) }

  | "M[" (num as n) "]"
    { get_token (MEM_ADDR'tok (to_num n)) }
  | "M[" (num as n) ':' val_size ']'
    { get_token (MEM_ADDR'tok (to_num n)) }
  | "M[X+" (num as n) ':' val_size ']'
    { get_token (MEM_ADDR'tok (to_num n)) }

  | newline
    { get_token NEWLINE'tok }
  | eof
    { get_token EOF'tok }

  (* A <- P[k:4] *)
  | "ld_word"
    { get_token LD_WORD'tok }
  (* A <- P[k:2] *)
  | "ld_half"
    { get_token LD_HALF'tok }
  (* A <- P[k:1] *)
  | "ld_byte"
    { get_token LD_BYTE'tok }
  (* A <- P[X+k:4] *)
  | "ld_ofst_word"
    { get_token LD_OFST_WORD'tok }
  (* A <- P[X+k:2] *)
  | "ld_ofst_half"
    { get_token LD_OFST_HALF'tok }
  (* A <- P[X+k:1] *)
  | "ldx_byte"
    { get_token LDX_BYTE'tok }
  (* A <- M[k] *)
  | "ld_mem"
    { get_token LD_MEM'tok }
  (* X <- M[k] *)
  | "ldx_mem"
    { get_token LDX_MEM'tok }
  (* M[k] <- A *)
  | "store"
    { get_token STORE'tok }
  (* M[k] <- X *)
  | "store_x"
    { get_token STORE_X'tok }
  (* Return, accept A bytes. *)
  | "ret_a"
    { get_token RET_A'tok }
  (* Return, accept k bytes. *)
  | "ret_k"
    { get_token RET_K'tok }
  (* X <- A *)
  | "x_store_a"
    { get_token X_STORE_A'tok }
  (* A <- X *)
  | "a_store_x"
    { get_token A_STORE_X'tok }
  (* X <- hdr_len *)
  (* doesn't fit well, needs to be special case *)
  (* so, we keep it as a lone opcode, exclude HDR_LEN *)
  (* was: X <- 4*(P[k:1]&0xf) *)
  | "ldx_hdr_len"
    { get_token LDX_HDR_LEN'tok }
  (* A <- k *)
  | "ld_imm"
    { get_token LD_IMM'tok }
  (* A <- A + k *)
  | "add_imm"
    { get_token ADD_IMM'tok }
  (* A <- A - k *)
  | "sub_imm"
    { get_token SUB_IMM'tok }
  (* A <- A * k *)
  | "mul_imm"
    { get_token MUL_IMM'tok }
  (* A <- A / k *)
  | "div_imm"
    { get_token DIV_IMM'tok }
  (* A <- A & k *)
  | "and_imm"
    { get_token AND_IMM'tok }
  (* A <- A | k *)
  | "or_imm"
    { get_token OR_IMM'tok }
  (* A <- A << k *)
  | "sl_imm"
    { get_token SL_IMM'tok }
  (* A <- A >> k *)
  | "sr_imm"
    { get_token SR_IMM'tok }
  (* A <- A + X *)
  | "add_x"
    { get_token ADD_X'tok }
  (* A <- A - X *)
  | "sub_x"
    { get_token SUB_X'tok }
  (* A <- A * X *)
  | "mul_x"
    { get_token MUL_X'tok }
  (* A <- A / X *)
  | "div_x"
    { get_token DIV_X'tok }
  (* A <- A & X *)
  | "and_x"
    { get_token AND_X'tok }
  (* A <- A | X *)
  | "or_x"
    { get_token OR_X'tok }
  (* A <- A << X *)
  | "sl_x"
    { get_token SL_X'tok }
  (* A <- A >> X *)
  | "sr_x"
    { get_token SR_X'tok }
  (* A <- -A *)
  | "neg"
    { get_token NEG'tok }
  (* pc += k *)
  | "jmp_imm"
    { get_token JMP_IMM'tok }
  (* X <- k *)
  | "ldx_imm"
    { get_token LDX_IMM'tok }
  (* pc += (A > k) ? jt : jf *)
  | "jgt_imm"
    { get_token JGT_IMM'tok }
  (* pc += (A >= k) ? jt : jf *)
  | "jge_imm"
    { get_token JGE_IMM'tok }
  (* pc += (A == k) ? jt : jf *)
  | "jeq_imm"
    { get_token JEQ_IMM'tok }
  (* pc += (A & k) ? jt : jf *)
  | "jand_imm"
    { get_token JAND_IMM'tok }
  (* A <- len *)
  | "ld_len"
    { get_token LD_LEN'tok }
  (* X <- len *)
  | "ldx_len"
    { get_token LDX_LEN'tok }
  (* pc += (A > X) ? jt : jf *)
  | "jgt_x"
    { get_token JGT_X'tok }
  (* pc += (A >= X) ? jt : jf *)
  | "jge_x"
    { get_token JGE_X'tok }
  (* pc += (A == X) ? jt : jf *)
  | "jeq_x"
    { get_token JEQ_X'tok }
  (* pc += (A & X) ? jt : jf *)
  | "jand_x"
    { get_token JAND_X'tok }

  | _
    { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

{
    let tokens_stream lexbuf : token stream =
        let rec compute_token_stream () =
            let loop c_exist =
                Cons (c_exist, Lazy.from_fun compute_token_stream)
            in loop (lex lexbuf)
        in
        Lazy.from_fun compute_token_stream
}
