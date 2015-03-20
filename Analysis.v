Require Import Parser.
Require Import Datatypes.
Require Import List.

(*
     "Each instruction performs some action on the pseudo-machine state, which
     consists of an accumulator, index register, scratch memory store, and
     implicit program counter."
        -OpenBSD man page
*)

Record state : Type := make_state {
    prev_instrs : list instr;
    curr_instr : instr;
    next_instrs : list instr;
    acc : imm;
    x_reg : imm;
    mem : list imm
}.

Definition step (s : state) : option state :=
    None.
