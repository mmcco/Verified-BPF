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


(*
  "Each instruction performs some action on the pseudo-machine state, which
   consists of an accumulator, index register, scratch memory store, and
   implicit program counter."
       -OpenBSD man page
*)


Definition scratch_mem := Vector.t (option (Word.word 32)) 16.

Definition empty_mem : scratch_mem :=
  Vector.const (None : option (Word.word 32)) 16.

Definition get_fin (i:nat) : option (Vectors.Fin.t 16) :=
  match Vectors.Fin.of_nat i 16 with
    | inleft x => Some x
    | _ => None
  end.

Record vm_state : Type := make_state {
  acc : option imm;
  x_reg : option imm;
  ins : list instr;
  pkt : list (Word.word 32);
  smem : scratch_mem
}.

Inductive end_state : Type :=
  | Ret : Word.word 32 -> end_state
  | Error : string -> end_state.

Inductive state : Type :=
  | ContState : vm_state -> state
  | End : end_state -> state.

Definition init_state (ins:list instr) : state :=
  ContState (make_state None None ins [] empty_mem).

Definition change_acc (s:vm_state) (new_acc:imm) : state :=
  ContState (make_state (Some new_acc) (x_reg s) (ins s) (pkt s) (smem s)).

Definition change_x_reg (s:vm_state) (new_x:imm) : state :=
  ContState (make_state (acc s) (Some new_x) (ins s) (pkt s) (smem s)).

Definition change_smem (s:vm_state) (i:nat) (v:Word.word 32) : state :=
    match get_fin i with
        | Some fin =>
            let new_mem := Vector.replace (smem s) fin (Some v) in
            ContState (make_state (acc s) (x_reg s) (ins s) (pkt s) new_mem)
        | None =>
            End (Error "accessed uninitialized memory")
    end.

Definition jump (s:vm_state) (n:word 32) : state :=
  ContState (make_state (acc s) (x_reg s) (skipn (wordToNat n) (ins s)) (pkt s) (smem s)).

Definition step (s:vm_state) : state :=
  match (ins s) with
    | [] => End (Error "jumped or stepped past last instruction")
    | (i :: rest) =>
    match i with
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
                    change_x_reg s acc'
                | None =>
                    End (Error "storing acc to uninitialized x reg")
              end
          | AStoreX =>
              match x_reg s with
                | Some x' =>
                    change_acc s x'
                | None =>
                    End (Error "storing x reg to uninitialized acc")
              end
              | AddX =>
                        match acc s, x_reg s with
                            | Some acc', Some x' =>
                                change_acc s ((acc') ^+ x')
                            | None, _ =>
                                End (Error "Adding to uninitialized acc")
                            | _, None =>
                                End (Error "Adding uninitialized x reg")
                        end
                    | SubX =>
                        match acc s, x_reg s with
                            | Some acc', Some x' =>
                                change_acc s ((acc') ^- x')
                            | None, _ =>
                                End (Error "Subtracting to uninitialized acc")
                            | _, None =>
                                End (Error "Subtracting uninitialized x reg")
                        end
                    | MulX =>
                        match acc s, x_reg s with
                            | Some acc', Some x' =>
                                change_acc s ((acc') ^* x')
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
                                change_acc s ((acc') ^& x')
                            | None, _ =>
                                End (Error "And-ing to uninitialized acc")
                            | _, None =>
                                End (Error "And-ing uninitialized x reg")
                        end
                    | OrX =>
                        match acc s, x_reg s with
                            | Some acc', Some x' =>
                                change_acc s ((acc') ^| x')
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
                        change_acc s pkt_len
                    | LdXLen =>
                        let pkt_len := Word.natToWord 32 (length (pkt s)) in
                        change_x_reg s pkt_len
                end
            | ImmInstr i_op i =>
                match i_op with
                    | RetK =>
                        End (Ret i)
                    | LdImm =>
                        change_acc s i
                    | AddImm =>
                        match acc s with
                            | Some acc' =>
                                change_acc s ((acc') ^+ i)
                            | None =>
                                End (Error "Adding to uninitialized acc")
                        end
                    | SubImm =>
                        match acc s with
                            | Some acc' =>
                                change_acc s ((acc') ^- i)
                            | None =>
                                End (Error "Subtracting to uninitialized acc")
                        end
                    | MulImm =>
                        match acc s with
                            | Some acc' =>
                                change_acc s ((acc') ^* i)
                            | None =>
                                End (Error "Multiplying to uninitialized acc")
                        end
                    | DivImm =>
                        End (Error "no div available for word (yet)")
                    | AndImm =>
                        match acc s with
                            | Some acc' =>
                                change_acc s ((acc') ^& i)
                            | None =>
                                End (Error "And-ing to uninitialized acc")
                        end
                    | OrImm =>
                        match acc s with
                            | Some acc' =>
                                change_acc s ((acc') ^| i)
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
                                change_acc s (wneg acc')
                            | None =>
                                End (Error "Adding to uninitialized acc")
                        end
                    | JmpImm =>
                        jump s i
                    | LdXImm =>
                        change_x_reg s i
                end
            | MemInstr m_op m_addr =>
                match m_op with
                    | LdMem =>
                        End (Error "*** fill in ***")
                    | LdXMem =>
                        End (Error "*** fill in ***")
                    | Store =>
                        End (Error "*** fill in ***")
                    | StoreX =>
                        End (Error "*** fill in ***")
                end
            | PktInstr p_op p_addr =>
                match p_op with
                    | LdWord =>
                        End (Error "*** fill in ***")
                    | LdHalf =>
                        End (Error "*** fill in ***")
                    | LdByte =>
                        End (Error "*** fill in ***")
                    | LdOfstWord =>
                        End (Error "*** fill in ***")
                    | LdOfstHalf =>
                        End (Error "*** fill in ***")
                    | LdXByte =>
                        End (Error "*** fill in ***")
                end
            (* "All conditionals use unsigned comparison conventions." *)
            | BrInstr b_op ofst1 ofst2 =>
                match b_op with
                    | JGTX =>
                         End (Error "*** fill in ***")
                        (*
                        match acc s, x_reg s with
                            | Some acc', Some x' =>
                                if wlt x' acc' then jump ofst1 else jump ofst2
                            | None, _ =>
                                End (Error "Testing uninitialized acc")
                            | _, None =>
                                End (Error "Testing uninitialized x reg")
                        end
                                *)
                    | JGEX =>
                        End (Error "*** fill in ***")
                    | JEqX =>
                        End (Error "*** fill in ***")
                    | JAndX =>
                        End (Error "*** fill in ***")
                end
            | ImmBrInstr i_b_op i ofst1 ofst2 =>
                match i_b_op with
                    | JGTImm =>
                        End (Error "*** fill in ***")
                    | JGEImm =>
                        End (Error "*** fill in ***")
                    | JEqImm =>
                        End (Error "*** fill in ***")
                    | JAndImm =>
                        End (Error "*** fill in ***")
                end
          end
  end.

Definition size (s:state) : nat :=
  match s with
    | End _ => 0
    | ContState cs => length (ins cs)
  end.

Program Fixpoint prog_eval (s: state) { measure (size s) } : end_state :=
  match s with
    | End (e_s) => e_s
    | ContState cs => prog_eval (step cs)
  end.

Next Obligation.
  induction (step cs).
  simpl.
  destruct cs. simpl.

(*
   Used to prove that offsets stay on word (and hence instruction)
   boundaries.
*)

Definition word_aligned sz (w : Word.word sz) : bool :=
    let n := Word.wordToNat w in
        (negb (Word.mod2 n)) && (negb (Word.mod2 (div2 n))).
