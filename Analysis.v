Require Import List.
Require Import Div2.
Require Import Bool.

Require Import Word.
Require Import Parser.
Require Import Datatypes.
Require IL.
Require NatMap.

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
    pkt : list imm;
    smem : list imm
}.

Definition step (s : state) : option state :=
    match curr_instr s with
        | SoloInstr s_op =>
            match s_op with
                | RetA =>
                    None
                | RetK =>
                    None
                | XStoreA =>
                    None
                | AStoreX =>
                    None
                | LdXHdrLen =>
                    None
                | LdLen =>
                    None
                | LdXLen =>
                    None
            end
        | ImmInstr i_op i =>
            match i_op with
                | LdImm =>
                    None
                | AddImm =>
                    None
                | SubImm =>
                    None
                | MulImm =>
                    None
                | DivImm =>
                    None
                | AndImm =>
                    None
                | OrImm =>
                    None
                | SLImm =>
                    None
                | SRImm =>
                    None
                | AddX =>
                    None
                | SubX =>
                    None
                | MulX =>
                    None
                | DivX =>
                    None
                | AndX =>
                    None
                | OrX =>
                    None
                | SLX =>
                    None
                | SRX =>
                    None
                | Neg =>
                    None
                | JmpImm =>
                    None
                | LdXImm =>
                    None
            end
        | MemInstr m_op m_addr =>
            match m_op with
                | LdMem =>
                    None
                | LdXMem =>
                    None
                | Store =>
                    None
                | StoreX =>
                    None
            end
        | PktInstr p_op p_addr =>
            match p_op with
                | LdWord =>
                    None
                | LdHalf =>
                    None
                | LdByte =>
                    None
                | LdOfstWord =>
                    None
                | LdOfstHalf =>
                    None
                | LdXByte =>
                    None
            end
        | BrInstr b_op ofst1 ofst2 =>
            match b_op with
                | JGTX =>
                    None
                | JGEX =>
                    None
                | JEqX =>
                    None
                | JAndX =>
                    None
            end
        | ImmBrInstr i_b_op i ofst1 ofst2 =>
            match i_b_op with
                | JGTImm =>
                    None
                | JGEImm =>
                    None
                | JEqImm =>
                    None
                | JAndImm =>
                    None
            end
    end.

(* Used to prove that offsets stay on word (and hence instruction)
   boundaries.
*)

Definition word_aligned sz (w : word sz) : bool :=
    (negb (mod2 (wordToNat w))) && (negb (mod2 (div2 (wordToNat w)))).

(*
Parameter mike_m : mem.
Parameter mike_a : addr.
Eval compute in (mem_get mike_m) mike_a.
*)