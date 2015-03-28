Require Import String.
Require Import Div2.
Require Import Bool.
Require Import List.
Require Vector.
Require Vectors.Fin.

Import ListNotations.

Require Import Word.
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
    pkt : list (Word.word 32);
    smem : scratch_mem
}.

Definition get_fin (i:nat) : option (Vectors.Fin.t 16) :=
    match Vectors.Fin.of_nat i 16 with
        | inleft x => Some x
        | _ => None
    end.


Definition change_acc (vms:vm_state) (new_acc:imm) :=
    make_state (Some new_acc) (x_reg vms) (pkt vms) (smem vms).

Definition change_x_reg (vms:vm_state) (new_x:imm) :=
    make_state (acc vms) (Some new_x) (pkt vms) (smem vms).

Definition change_smem (vms:vm_state) (i:nat) (v:Word.word 32) : option vm_state :=
    match get_fin i with
        | Some fin =>
            let new_mem := Vector.replace (smem vms) fin (Some v) in
            Some (make_state (acc vms) (x_reg vms) (pkt vms) new_mem)
        | None => None
    end.

Inductive end_state : Type :=
    | Ret : Word.word 32 -> end_state
    | Error : string -> end_state.

Inductive state : Type :=
    | ContState : vm_state -> state
    | End : end_state -> state.

Definition init_state : state :=
            ContState (make_state None None [] empty_mem).

Definition get_error (str:string) :=
    (End (Error str), 1).

Definition single_step (s:vm_state) : state * nat :=
    (ContState s, 1).

Definition jump (s:vm_state) (n:word 32) : state * nat :=
    (ContState s, (wordToNat n) + 1).

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
                    match acc s with
                        | Some acc' =>
                            single_step (change_x_reg s acc')
                        | None =>
                            get_error "storing acc to uninitialized x reg"
                    end
                | AStoreX =>
                    match x_reg s with
                        | Some x' =>
                            single_step (change_acc s x')
                        | None =>
                            get_error "storing x reg to uninitialized acc"
                    end
                | LdXHdrLen =>
                    get_error "*** fill in ***"
                | LdLen =>
                    let pkt_len := Word.natToWord 32 (length (pkt s)) in
                    single_step (change_acc s pkt_len)
                | LdXLen =>
                    let pkt_len := Word.natToWord 32 (length (pkt s)) in
                    single_step (change_x_reg s pkt_len)
            end
        | ImmInstr i_op i =>
            match i_op with
                | RetK =>
                    (End (Ret i), 1)
                | LdImm =>
                    single_step (change_acc s i)
                | AddImm =>
                    match acc s with
                        | Some acc' =>
                            single_step (change_acc s ((acc') ^+ i))
                        | None =>
                            get_error "Adding to uninitialized acc"
                    end
                | SubImm =>
                    match acc s with
                        | Some acc' =>
                            single_step (change_acc s ((acc') ^- i))
                        | None =>
                            get_error "Subtracting to uninitialized acc"
                    end
                | MulImm =>
                    match acc s with
                        | Some acc' =>
                            single_step (change_acc s ((acc') ^* i))
                        | None =>
                            get_error "Multiplying to uninitialized acc"
                    end
                | DivImm =>
                    get_error "no div available for word (yet)"
                | AndImm =>
                    match acc s with
                        | Some acc' =>
                            single_step (change_acc s ((acc') ^& i))
                        | None =>
                            get_error "And-ing to uninitialized acc"
                    end
                | OrImm =>
                    match acc s with
                        | Some acc' =>
                            single_step (change_acc s ((acc') ^| i))
                        | None =>
                            get_error "Or-ing to uninitialized acc"
                    end
                | SLImm =>
                    get_error "no shifts available yet"
                | SRImm =>
                    get_error "no shifts available yet"
                | AddX =>
                    match acc s, x_reg s with
                        | Some acc', Some x' =>
                            single_step (change_acc s ((acc') ^+ x'))
                        | None, _ =>
                            get_error "Adding to uninitialized acc"
                        | _, None =>
                            get_error "Adding uninitialized x reg"
                    end
                | SubX =>
                    match acc s, x_reg s with
                        | Some acc', Some x' =>
                            single_step (change_acc s ((acc') ^- x'))
                        | None, _ =>
                            get_error "Subtracting to uninitialized acc"
                        | _, None =>
                            get_error "Subtracting uninitialized x reg"
                    end
                | MulX =>
                    match acc s, x_reg s with
                        | Some acc', Some x' =>
                            single_step (change_acc s ((acc') ^* x'))
                        | None, _ =>
                            get_error "Multiplying to uninitialized acc"
                        | _, None =>
                            get_error "Multiplying uninitialized x reg"
                    end
                | DivX =>
                    (End (Error "*** fill in ***"), 1)
                | AndX =>
                    match acc s, x_reg s with
                        | Some acc', Some x' =>
                            single_step (change_acc s ((acc') ^& x'))
                        | None, _ =>
                            get_error "And-ing to uninitialized acc"
                        | _, None =>
                            get_error "And-ing uninitialized x reg"
                    end
                | OrX =>
                    match acc s, x_reg s with
                        | Some acc', Some x' =>
                            single_step (change_acc s ((acc') ^| x'))
                        | None, _ =>
                            get_error "Or-ing to uninitialized acc"
                        | _, None =>
                            get_error "Or-ing uninitialized x reg"
                    end
                | SLX =>
                    get_error "no shifts available yet"
                | SRX =>
                    get_error "no shifts available yet"
                | Neg =>
                    match acc s with
                        | Some acc' =>
                            single_step (change_acc s (wneg acc'))
                        | None =>
                            get_error "Adding to uninitialized acc"
                    end
                | JmpImm =>
                    jump s i
                | LdXImm =>
                    single_step (change_acc s i)
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
