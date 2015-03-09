Require Import List.

(*
   The state of a BPF VM is contained in the following information:
     * the remaining instructions (can only jump/branch forward)
       - the current instruction is the first in this list
     * the accumulator value
     * the index register value X
     * the memory state M
     * the packet value P
       - immutable
*)

Inductive opcode : Type :=
  (*
     Loads to accumulator
  *)
  (* A <- P[k:4] *)
  | ld_word : opcode
  (* A <- P[k:2] *)
  | ld_half : opcode
  (* A <- P[k:1] *)
  | ld_byte : opcode
  (* A <- P[X+k:4] *)
  | ld_ofst_word : opcode
  (* A <- P[X+k:2] *)
  | ld_ofst_half : opcode
  (* A <- P[X+k:1] *)
  | ldx_byte : opcode
  (* A <- len *)
  | ld_len : opcode
  (* A <- k *)
  | ld_imm : opcode
  (* A <- M[k] *)
  | ld_mem : opcode

  (*
     Loads to index register
  *)
  (* X <- k *)
  | ldx_imm : opcode
  (* X <- M[k] *)
  | ldx_mem : opcode
  (* X <- len *)
  | ldx_len : opcode
  (* X <- 4*(P[k:1]&0xf) *)
  | ldx_hdr_len : opcode

  (* M[k] <- A *)
  | store : opcode

  (* M[k] <- X *)
  | store_x : opcode

  (* A <- A + k *)
  | add_imm : opcode
  (* A <- A - k *)
  | sub_imm : opcode
  (* A <- A * k *)
  | mul_imm : opcode
  (* A <- A / k *)
  | div_imm : opcode
  (* A <- A & k *)
  | and_imm : opcode
  (* A <- A | k *)
  | or_imm : opcode
  (* A <- A << k *)
  | sl_imm : opcode
  (* A <- A >> k *)
  | sr_imm : opcode
  (* A <- A + X *)
  | add_x : opcode
  (* A <- A - X *)
  | sub_x : opcode
  (* A <- A * X *)
  | mul_x : opcode
  (* A <- A / X *)
  | div_x : opcode
  (* A <- A & X *)
  | and_x : opcode
  (* A <- A | X *)
  | or_x : opcode
  (* A <- A << X *)
  | sl_x : opcode
  (* A <- A >> X *)
  | sr_x : opcode
  (* A <- -A *)
  | neg : opcode

  (* pc += k *)
  | jmp_imm : opcode
  (* pc += (A > k) ? jt : jf *)
  | jgt_imm : opcode
  (* pc += (A >= k) ? jt : jf *)
  | jge_imm : opcode
  (* pc += (A == k) ? jt : jf *)
  | jeq_imm : opcode
  (* pc += (A & k) ? jt : jf *)
  | jand_imm : opcode
  (* pc += (A > X) ? jt : jf *)
  | jgt_x : opcode
  (* pc += (A >= X) ? jt : jf *)
  | jge_x : opcode
  (* pc += (A == X) ? jt : jf *)
  | jeq_x : opcode
  (* pc += (A & X) ? jt : jf *)
  | jand_x : opcode

  (* Return, accept A bytes. *)
  | ret_a : opcode
  (* Return, accept k bytes. *)
  | ret_k : opcode

  (* X <- A *)
  | x_store_a : opcode
  (* A <- X *)
  | a_store_x : opcode.

(*
   The VM is explicitly always 32-bit, so we can hard-code these.
*)
Inductive val : Type :=
  | word : nat -> val
  | half : nat -> val
  | byte : nat -> val.

Inductive operand : Type :=
  | imm : nat -> operand
  | reg : nat -> operand
  | mem : val -> operand
  | pkt : nat -> operand.

(*
   We use OpenBSD's bpf man page for our definitions.

   struct bpf_insn { 
       u_int16_t  code; 
       u_char     jt; 
       u_char     jf; 
       u_int32_t  k; 
   };

   At least for now, we use options for the integers' types because
   for some instructions the integers are unused.
*)
Inductive instr : Type :=
  | opcode -> option nat -> option nat -> option nat.

(*
  OpenBSD man page:
    "The bpf interface provides the following macros
    to facilitate array initializers:"
*)
Inductive macro : Type :=
  | stmt : opcode -> operand -> macro
  | jump : opcode -> operand -> nat -> nat -> macro.

Definition compile_macro (m : macro) : instr :=
  match macro with
  | stmt opcode' operand' =>
    ld_imm None None None
  | jump opcode' operand' true_offset false_offset =>
    store None None None
  end.

(*
   Things to prove about jumps:
     * the offset and base address are 0 mod 4
     * the resulting address is 0 mod 4
       - (implied by above assuming semantic preservation)
     * jump will always add at least 4 to PC
     * jump will always land within existing code
*)

(*
   Every packet is returned with the following header:

   struct bpf_hdr {
       struct bpf_timeval bh_tstamp;
       u_int32_t  bh_caplen;
       u_int32_t  bh_datalen;
       u_int16_t  bh_hdrlen;
   };

   We could prove correctness properties about it.
*)

(*
   The bpf program ultimately returned is comprised of a list of
   instructions and that list's length:

   struct bpf_program {
       u_int bf_len;
       struct bpf_insn *bf_insns;
   };
*)