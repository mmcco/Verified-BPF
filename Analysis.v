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
    curr_instr : instr;
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

Definition init_state (ins : list instr) : state :=
    match hd_error ins with
        | None =>
            End (Error "empty instruction list")
        | Some i =>
            ContState (make_state i None None empty_mem empty_mem)
    end.



Definition step (s : vm_state) : state * nat :=
    match curr_instr s with
        | SoloInstr s_op =>
            match s_op with
                | RetA =>
                    (End (Error "*** fill in ***"), 1)
                | RetK =>
                    (End (Error "*** fill in ***"), 1)
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

(*
Fixpoint beq_nat (n m : nat) : bool :=
  match n with
  | O => match m with
         | O => true
         | S m' => false
         end
  | S n' => match m with
            | O => false
            | S m' => beq_nat n' m'
            end
  end.

Definition beq_nat 

Variable A : Type.
Require Import Bool.
Theorem not_zero : forall (n:nat), beq_nat (S n) 0 = false.
simpl. reflexivity. Qed.

Theorem skipn_dec : forall (n:nat) (l:list A), gt (length l) 0 -> lt (length (skipn (S n) l)) (length l).
Proof.
rewrite -> not_zero.
simpl.
intuition.
rewrite -> not_zero.
Qed.

Variable A : Type.

Definition lte (a b : nat) := or (lt a b) (eq a b).

Lemma ref : forall (n:nat), eq n n.
reflexivity.
Qed.

Lemma refl : forall (n:nat), lte n n.
destruct n as [| n']. rewrite -> ref. reflexivity.
Qed.

Theorem skipn_dec : forall (n:nat) (l:list A), lte (length (skipn n l)) (length l).

destruct n as [| n']. simpl. rewrite -> lte. reflexivity.*)

Lemma skipn_dec : forall A (n:nat) (l:list A), length l >= length (skipn n l).
Proof.
intros A n l.
induction l as [| hd tl].
intro Hn.


simpl.
destruct n as [| n'].
simpl.
intuition.
intuition.
rewrite hi.

intuition.
destruct n as [| n'].
simpl.
intuition.
simpl.
case [].
induction l.
reflexivity.
induction l.
simpl.
reflexivity.
Qed.


Fixpoint prog_eval (ins : list instr) (s : state) : end_state :=
    match ins with
        | next_i :: rest =>
            match s with
            | ContState vms =>
                match step vms with
                    | (_, 0) => Error "step size of zero"
                    | (vms', step_size) => prog_eval rest vms'
                end
            | End end_s => end_s
            end
        | [] =>
            Error "empty instr list, never reached a return"
    end.
(*
    match s with
        | ContState vms =>
            match step vms with
                | (_, 0) => Error "step size of zero"
                | (vms', step_size) => prog_eval (skipn step_size ins) vms'
            end
        | End end_s =>
            end_s
    end.*)

(* Used to prove that offsets stay on word (and hence instruction)
   boundaries.
*)

Definition word_aligned sz (w : word sz) : bool :=
    (negb (mod2 (wordToNat w))) && (negb (mod2 (div2 (wordToNat w)))).
