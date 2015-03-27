Require Import String.
Require Import Div2.
Require Import Bool.
Require Import List.
Require Vector.

Import ListNotations.

Require Word.
Require Import Parser.


(*
     "Each instruction performs some action on the pseudo-machine state, which
     consists of an accumulator, index register, scratch memory store, and
     implicit program counter."
        -OpenBSD man page
*)


Definition scratch_mem := Vector.t (option (Word.word 32)) 16.

Definition empty_mem : scratch_mem :=
    Vector.const (None : option (Word.word 32)) 16.

Record vm_state : Type := make_state {
    (* Future instructions kept separate for ease of Fixpoint defs *)
    acc : option imm;
    x_reg : option imm;
    pkt : scratch_mem;
    smem : scratch_mem
}.

Inductive end_state : Type :=
    | Ret : Word.word 32 -> end_state
    | Error : string -> end_state.

Inductive state : Type :=
    | ContState : vm_state -> state
    | End : end_state -> state.

Definition init_state : state :=
            ContState (make_state None None empty_mem empty_mem).



Definition step (s:vm_state) (i:instr) (ins:list instr) : state * nat :=
    match i with
        | SoloInstr s_op =>
            match s_op with
                | RetA =>
                    match acc s with
                        | None => (End (Error "Returned uninitialized acc"), 1)
                        | Some v => (End (Ret v), 1)
                    end
                | XStoreA =>
                    (End (Error "*** fill in ***"), 1)
                | AStoreX =>
                    (End (Error "*** fill in ***"), 1)
                | LdXHdrLen =>
                    (End (Error "*** fill in ***"), 1)
                | LdLen =>
                    (End (Error "*** fill in ***"), 1)
                | LdXLen =>
                    (End (Error "*** fill in ***"), 1)
            end
        | ImmInstr i_op i =>
            match i_op with
                | RetK =>
                    (End (Error "*** fill in ***"), 1)
                | LdImm =>
                    (End (Error "*** fill in ***"), 1)
                | AddImm =>
                    (End (Error "*** fill in ***"), 1)
                | SubImm =>
                    (End (Error "*** fill in ***"), 1)
                | MulImm =>
                    (End (Error "*** fill in ***"), 1)
                | DivImm =>
                    (End (Error "*** fill in ***"), 1)
                | AndImm =>
                    (End (Error "*** fill in ***"), 1)
                | OrImm =>
                    (End (Error "*** fill in ***"), 1)
                | SLImm =>
                    (End (Error "*** fill in ***"), 1)
                | SRImm =>
                    (End (Error "*** fill in ***"), 1)
                | AddX =>
                    (End (Error "*** fill in ***"), 1)
                | SubX =>
                    (End (Error "*** fill in ***"), 1)
                | MulX =>
                    (End (Error "*** fill in ***"), 1)
                | DivX =>
                    (End (Error "*** fill in ***"), 1)
                | AndX =>
                    (End (Error "*** fill in ***"), 1)
                | OrX =>
                    (End (Error "*** fill in ***"), 1)
                | SLX =>
                    (End (Error "*** fill in ***"), 1)
                | SRX =>
                    (End (Error "*** fill in ***"), 1)
                | Neg =>
                    (End (Error "*** fill in ***"), 1)
                | JmpImm =>
                    (End (Error "*** fill in ***"), 1)
                | LdXImm =>
                    (End (Error "*** fill in ***"), 1)
            end
        | MemInstr m_op m_addr =>
            match m_op with
                | LdMem =>
                    (End (Error "*** fill in ***"), 1)
                | LdXMem =>
                    (End (Error "*** fill in ***"), 1)
                | Store =>
                    (End (Error "*** fill in ***"), 1)
                | StoreX =>
                    (End (Error "*** fill in ***"), 1)
            end
        | PktInstr p_op p_addr =>
            match p_op with
                | LdWord =>
                    (End (Error "*** fill in ***"), 1)
                | LdHalf =>
                    (End (Error "*** fill in ***"), 1)
                | LdByte =>
                    (End (Error "*** fill in ***"), 1)
                | LdOfstWord =>
                    (End (Error "*** fill in ***"), 1)
                | LdOfstHalf =>
                    (End (Error "*** fill in ***"), 1)
                | LdXByte =>
                    (End (Error "*** fill in ***"), 1)
            end
        | BrInstr b_op ofst1 ofst2 =>
            match b_op with
                | JGTX =>
                    (End (Error "*** fill in ***"), 1)
                | JGEX =>
                    (End (Error "*** fill in ***"), 1)
                | JEqX =>
                    (End (Error "*** fill in ***"), 1)
                | JAndX =>
                    (End (Error "*** fill in ***"), 1)
            end
        | ImmBrInstr i_b_op i ofst1 ofst2 =>
            match i_b_op with
                | JGTImm =>
                    (End (Error "*** fill in ***"), 1)
                | JGEImm =>
                    (End (Error "*** fill in ***"), 1)
                | JEqImm =>
                    (End (Error "*** fill in ***"), 1)
                | JAndImm =>
                    (End (Error "*** fill in ***"), 1)
            end
    end.


Fixpoint prog_eval (ins:list instr) (s:state) (steps:nat) : end_state :=
    match s with
        | End (e_s) => e_s
        | ContState vms =>
            match ins with
                | next_i :: rest =>
                    match steps with
                    | 0 => Error "step size of zero"
                    | 1 =>
                        match s with
                            | ContState vms =>
                                let (vms', s_sz) := step vms next_i rest in
                                prog_eval rest vms' s_sz
                            | End end_s => end_s
                        end
                    | S n' => prog_eval rest s n'
                end
                | [] => Error "empty instr list, never reached a return"
            end
    end.

(* Used to prove that offsets stay on word (and hence instruction)
   boundaries.
*)

Definition word_aligned sz (w : Word.word sz) : bool :=
    let n := Word.wordToNat w in
        (negb (Word.mod2 n)) && (negb (Word.mod2 (div2 n))).
