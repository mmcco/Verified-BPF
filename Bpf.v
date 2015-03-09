Require Import String.

Inductive rel_addr : Type :=
    | pkt_addr : nat -> rel_addr
    | mem_addr : nat -> rel_addr.

Inductive instr : Type :=
    | solo_instr : string -> instr
    | imm_instr : string -> nat -> instr
    | offset_instr : string -> rel_addr -> instr
    | imm_br_instr : string -> nat -> nat -> nat -> instr
    | br_instr : string -> nat -> nat -> instr
    | len_instr : string -> instr.
