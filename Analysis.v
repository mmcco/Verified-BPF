Require Import Arith.
Require Import String.
Require Import Div2.
Require Import Bool.
Require Import List.
Require Import Program.
Require Vector.
Require Vectors.Fin.

Import ListNotations.

Require Import Word.
Require Import Parser.
Require Skipn.

Definition W := Word.word 32.
Definition H := Word.word 16.
Definition B := Word.word 8.
Definition W_zero := (natToWord 32 0).
Definition H_zero := (natToWord 16 0).
Definition B_zero := (natToWord 8 0).

Definition BtoW (b:B) : W :=
  combine (combine b B_zero) (natToWord 16 0).

Definition BtoH (b:B) : H :=
  combine b B_zero.

Definition HtoW (h:H) : W :=
  combine H_zero h.

(*
    Bytes are given in big-endian (aka normal human being) order
    but must be processed in little-endian order.
*)
Definition combine_bs (a b c d : B) : W :=
  combine (combine d c) (combine b a).

(*
  "Each instruction performs some action on the pseudo-machine state, which
   consists of an accumulator, index register, scratch memory store, and
   implicit program counter."
       -OpenBSD man page
*)

Fixpoint ble_nat (n m : nat) : bool :=
  match n with
    | O => true
    | S n' =>
        match m with
          | O => false
          | S m' => ble_nat n' m'
        end
    end.

Fixpoint blt_nat (n m : nat) : bool :=
  match n with
    | O => if beq_nat m O then false else true
    | S n' =>
        match m with
          | O => false
          | S m' => ble_nat n' m'
        end
    end.

Fixpoint bgt_nat (n m : nat) : bool :=
  match n with
    | O => false
    | S n' =>
        match m with
          | O => true
          | S m' => ble_nat n' m'
        end
    end.

Fixpoint bge_nat (n m : nat) : bool :=
  match n with
    | O => if beq_nat m O then true else false
    | S n' =>
        match m with
          | O => true
          | S m' => ble_nat n' m'
        end
    end.

Definition scratch_mem := Vector.t (option W) 16.

Definition empty_mem : scratch_mem :=
  Vector.const None 16.

Definition get_fin (i:nat) : option (Vectors.Fin.t 16) :=
  match Vectors.Fin.of_nat i 16 with
    | inleft x => Some x
    | _ => None
  end.

Record vm_state : Type := make_state {
  acc : option W;
  x_reg : option W;
  ins : list instr;
  pkt : list B;
  smem : scratch_mem
}.

Inductive end_state : Type :=
  | Ret : W -> end_state
  | Error : string -> end_state.

Inductive state : Type :=
  | ContState : vm_state -> state
  | End : end_state -> state.

Definition init_state (ins:list instr) : vm_state :=
  make_state None None ins [] empty_mem.

Definition change_acc (s:vm_state) (l:list instr) (new_acc:W) : state :=
  ContState (make_state (Some new_acc) (x_reg s) l (pkt s) (smem s)).

Definition change_x_reg (s:vm_state) (l:list instr) (new_x:W) : state :=
  ContState (make_state (acc s) (Some new_x) l (pkt s) (smem s)).

Definition change_smem (s:vm_state) (l:list instr) (i:W) (v:W) : state :=
  match get_fin (wordToNat i) with
    | Some fin =>
        let new_mem := Vector.replace (smem s) fin (Some v) in
          ContState (make_state (acc s) (x_reg s) l (pkt s) new_mem)
    | None =>
        End (Error "accessed uninitialized memory")
  end.

Definition get_mem_ind (l:scratch_mem) (i:W) : option W :=
  match get_fin (wordToNat i) with
    | None => None
    | Some f => Vector.nth l f
  end.

Definition jump (s:vm_state) (n:W) : state :=
  ContState (make_state (acc s) (x_reg s) (skipn (wordToNat n) (ins s)) (pkt s) (smem s)).

Fixpoint step (s:vm_state) : state :=
  match (ins s) with
    | [] => End (Error "jumped or stepped past last instruction")
    | (curr_instr :: rest) =>
    match curr_instr with
      | SoloInstr s_op =>
        match s_op with
          | RetA =>
              match acc s with
                | None => End (Error "Returned uninitialized acc")
                | Some v => End (Ret v)
              end
          | XStoreA =>
              match acc s with
                | Some acc' =>
                    change_x_reg s rest acc'
                | None =>
                    End (Error "storing acc to uninitialized x reg")
              end
          | AStoreX =>
              match x_reg s with
                | Some x' =>
                    change_acc s rest x'
                | None =>
                    End (Error "storing x reg to uninitialized acc")
              end
              | AddX =>
                        match acc s, x_reg s with
                            | Some acc', Some x' =>
                                change_acc s rest (acc' ^+ x')
                            | None, _ =>
                                End (Error "Adding to uninitialized acc")
                            | _, None =>
                                End (Error "Adding uninitialized x reg")
                        end
                    | SubX =>
                        match acc s, x_reg s with
                            | Some acc', Some x' =>
                                change_acc s rest (acc' ^- x')
                            | None, _ =>
                                End (Error "Subtracting to uninitialized acc")
                            | _, None =>
                                End (Error "Subtracting uninitialized x reg")
                        end
                    | MulX =>
                        match acc s, x_reg s with
                            | Some acc', Some x' =>
                                change_acc s rest (acc' ^* x')
                            | None, _ =>
                                End (Error "Multiplying to uninitialized acc")
                            | _, None =>
                                End (Error "Multiplying uninitialized x reg")
                        end
                    | DivX =>
                        End (Error "*** fill in ***")
                    | AndX =>
                        match acc s, x_reg s with
                            | Some acc', Some x' =>
                                change_acc s rest (acc' ^& x')
                            | None, _ =>
                                End (Error "And-ing to uninitialized acc")
                            | _, None =>
                                End (Error "And-ing uninitialized x reg")
                        end
                    | OrX =>
                        match acc s, x_reg s with
                            | Some acc', Some x' =>
                                change_acc s rest (acc' ^| x')
                            | None, _ =>
                                End (Error "Or-ing to uninitialized acc")
                            | _, None =>
                                End (Error "Or-ing uninitialized x reg")
                        end
                    | SLX =>
                        End (Error "no shifts available yet")
                    | SRX =>
                        End (Error "no shifts available yet")
                    | LdXHdrLen =>
                        End (Error "*** fill in ***")
                    | LdLen =>
                        let pkt_len := Word.natToWord 32 (length (pkt s)) in
                        change_acc s rest pkt_len
                    | LdXLen =>
                        let pkt_len := Word.natToWord 32 (length (pkt s)) in
                        change_x_reg s rest pkt_len
                end
            | ImmInstr i_op i =>
                match i_op with
                    | RetK =>
                        End (Ret i)
                    | LdImm =>
                        change_acc s rest i
                    | AddImm =>
                        match acc s with
                            | Some acc' =>
                                change_acc s rest ((acc') ^+ i)
                            | None =>
                                End (Error "Adding to uninitialized acc")
                        end
                    | SubImm =>
                        match acc s with
                            | Some acc' =>
                                change_acc s rest ((acc') ^- i)
                            | None =>
                                End (Error "Subtracting to uninitialized acc")
                        end
                    | MulImm =>
                        match acc s with
                            | Some acc' =>
                                change_acc s rest ((acc') ^* i)
                            | None =>
                                End (Error "Multiplying to uninitialized acc")
                        end
                    | DivImm =>
                        End (Error "no div available for word (yet)")
                    | AndImm =>
                        match acc s with
                            | Some acc' =>
                                change_acc s rest ((acc') ^& i)
                            | None =>
                                End (Error "And-ing to uninitialized acc")
                        end
                    | OrImm =>
                        match acc s with
                            | Some acc' =>
                                change_acc s rest ((acc') ^| i)
                            | None =>
                                End (Error "Or-ing to uninitialized acc")
                        end
                    | SLImm =>
                        End (Error "no shifts available yet")
                    | SRImm =>
                        End (Error "no shifts available yet")
                    | Neg =>
                        match acc s with
                            | Some acc' =>
                                change_acc s rest (wneg acc')
                            | None =>
                                End (Error "Adding to uninitialized acc")
                        end
                    | JmpImm =>
                        jump s i
                    | LdXImm =>
                        change_x_reg s rest i
                end
            | MemInstr m_op m_addr =>
                match m_op with
                    | LdMem =>
                        match get_mem_ind (smem s) m_addr with
                          | None => End (Error "access to undefined or out-of-bounds mem")
                          | Some a => change_acc s rest a
                        end
                    | LdXMem =>
                        match get_mem_ind (smem s) m_addr with
                          | None => End (Error "access to undefined or out-of-bounds mem")
                          | Some x => change_x_reg s rest x
                        end
                    | Store =>
                        match acc s with
                          | None => End (Error "using uninitialized acc as mem addr")
                          | Some a => change_smem s rest m_addr a
                        end
                    | StoreX =>
                        match x_reg s with
                          | None => End (Error "using uninitialized x reg as mem addr")
                          | Some x => change_smem s rest m_addr x
                        end
                end
            | PktInstr p_op p_addr =>
                match p_op with
                    | LdWord =>
                        let fst_b := nth_error (pkt s) (wordToNat p_addr) in
                        let snd_b := nth_error (pkt s) ((wordToNat p_addr) + 1) in
                        let thr_b := nth_error (pkt s) ((wordToNat p_addr) + 2) in
                        let frt_b := nth_error (pkt s) ((wordToNat p_addr) + 3) in
                        match fst_b, snd_b, thr_b, frt_b with
                          | Some a, Some b, Some c, Some d =>
                              change_acc s rest (combine_bs a b c d)
                          | _, _, _ ,_ =>
                              End (Error "out-of-bounds packet access")
                        end
                    | LdHalf =>
                        let fst_b := nth_error (pkt s) (wordToNat p_addr) in
                        let snd_b := nth_error (pkt s) ((wordToNat p_addr) + 1) in
                        match fst_b, snd_b with
                          | Some a, Some b =>
                              change_acc s rest (HtoW (combine b a))
                          | _, _ =>
                              End (Error "out-of-bounds packet access")
                        end
                    | LdByte =>
                        match nth_error (pkt s) (wordToNat p_addr) with
                          | None => End (Error "out-of-bounds packet access")
                          | Some a => change_acc s rest (BtoW a)
                        end
                    | LdOfstWord =>
                        End (Error "*** fill in ***")
                    | LdOfstHalf =>
                        End (Error "*** fill in ***")
                    | LdXByte =>
                        match nth_error (pkt s) (wordToNat p_addr) with
                          | None => End (Error "out-of-bounds packet access")
                          | Some x => change_x_reg s rest (BtoW x)
                        end
                end
            (* "All conditionals use unsigned comparison conventions." *)
            | BrInstr b_op ofst1 ofst2 =>
                match b_op with
                    | JGTX =>
                        match acc s, x_reg s with
                            | Some acc', Some x' =>
                                if bgt_nat (wordToNat acc') (wordToNat x')
                                  then jump s (BtoW ofst1)
                                else jump s (BtoW ofst2)
                            | None, _ =>
                                End (Error "Testing uninitialized acc")
                            | _, None =>
                                End (Error "Testing uninitialized x reg")
                        end
                    | JGEX =>
                        match acc s, x_reg s with
                            | Some acc', Some x' =>
                                if bge_nat (wordToNat acc') (wordToNat x')
                                  then jump s (BtoW ofst1)
                                else jump s (BtoW ofst2)
                            | None, _ =>
                                End (Error "Testing uninitialized acc")
                            | _, None =>
                                End (Error "Testing uninitialized x reg")
                        end
                    | JEqX =>
                        match acc s, x_reg s with
                            | Some acc', Some x' =>
                                if beq_nat (wordToNat acc') (wordToNat x')
                                  then jump s (BtoW ofst1)
                                else jump s (BtoW ofst2)
                            | None, _ =>
                                End (Error "Testing uninitialized acc")
                            | _, None =>
                                End (Error "Testing uninitialized x reg")
                        end
                    | JAndX =>
                        match acc s, x_reg s with
                            | Some acc', Some x' =>
                                if weqb (acc' ^& x') W_zero
                                  then jump s (BtoW ofst2) (* note reversed branches *)
                                else jump s (BtoW ofst1)
                            | None, _ =>
                                End (Error "Testing uninitialized acc")
                            | _, None =>
                                End (Error "Testing uninitialized x reg")
                        end
                end
            | ImmBrInstr i_b_op i ofst1 ofst2 =>
                match i_b_op with
                    | JGTImm =>
                        match acc s, i with
                            | Some acc', i' =>
                                if bgt_nat (wordToNat acc') (wordToNat i')
                                  then jump s (BtoW ofst1)
                                else jump s (BtoW ofst2)
                            | None, _ =>
                                End (Error "Testing uninitialized acc")
                        end
                    | JGEImm =>
                        match acc s, i with
                            | Some acc', i' =>
                                if bge_nat (wordToNat acc') (wordToNat i')
                                  then jump s (BtoW ofst1)
                                else jump s (BtoW ofst2)
                            | None, _ =>
                                End (Error "Testing uninitialized acc")
                        end
                    | JEqImm =>
                        match acc s, i with
                            | Some acc', i' =>
                                if beq_nat (wordToNat acc') (wordToNat i')
                                  then jump s (BtoW ofst1)
                                else jump s (BtoW ofst2)
                            | None, _ =>
                                End (Error "Testing uninitialized acc")
                        end
                    | JAndImm =>
                        match acc s, i with
                            | Some acc', i' =>
                                if weqb (acc' ^& i') W_zero
                                  then jump s (BtoW ofst2) (* note reversed branches *)
                                else jump s (BtoW ofst1)
                            | None, _ =>
                                End (Error "Testing uninitialized acc")
                        end
                end
          end
  end.

Fixpoint prog_eval (fuel:nat) (s:vm_state) : end_state :=
  match fuel with
    | O => Error "ran out of fuel"
    | S n =>
        match step s with
          | End e_s => e_s
          | ContState cs => prog_eval n cs
        end
  end.

Definition run_filter (l:list instr) : end_state :=
  prog_eval (length l) (init_state l).

(*
   Used to prove that offsets stay on word (and hence instruction)
   boundaries.
*)
Definition word_aligned sz (w : Word.word sz) : bool :=
    let n := Word.wordToNat w in
        (negb (Word.mod2 n)) && (negb (Word.mod2 (div2 n))).
