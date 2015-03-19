%{

Require Import String.

Definition imm : Type := nat % type.
Definition pkt_ofst : Type := nat % type.
Definition mem_ofst : Type := nat % type.
Definition offset : Type := nat % type.
Parameter terminal : Type.

Inductive solo_op :=
    | RetA
    | RetK
    | XStoreA
    | AStoreX
    | LdXHdrLen
    | LdLen
    | LdXLen.

Inductive imm_op :=
    | LdImm
    | AddImm
    | SubImm
    | MulImm
    | DivImm
    | AndImm
    | OrImm
    | SLImm
    | SRImm
    | AddX
    | SubX
    | MulX
    | DivX
    | AndX
    | OrX
    | SLX
    | SRX
    | Neg
    | JmpImm
    | LdXImm.

Inductive mem_op :=
    | LdMem
    | LdXMem
    | Store
    | StoreX.

Inductive pkt_op :=
    | LdWord
    | LdHalf
    | LdByte
    | LdOfstWord
    | LdOfstHalf
    | LdXByte.

Inductive br_op :=
    | JGTX
    | JGEX
    | JEqX
    | JAndX.

Inductive imm_br_op :=
    | JGTImm
    | JGEImm
    | JEqImm
    | JAndImm.

Inductive instr :=
    | SoloInstr : solo_op -> instr
    | ImmInstr : imm_op -> imm -> instr
    | MemInstr : mem_op -> mem_ofst -> instr
    | PktInstr : pkt_op -> pkt_ofst -> instr
    | BrInstr : br_op -> offset -> offset -> instr
    | ImmBrInstr : imm_br_op -> imm -> offset -> offset -> instr.

%}

(* We begin with all the opcode tokens *)

%token LD_WORD
%token LD_HALF
%token LD_BYTE
%token LD_OFST_WORD
%token LD_OFST_HALF
%token LDX_BYTE
%token LD_MEM
%token LDX_MEM
%token STORE
%token STORE_X
%token RET_A
%token RET_K
%token X_STORE_A
%token A_STORE_X
%token LDX_HDR_LEN
%token LD_IMM
%token ADD_IMM
%token SUB_IMM
%token MUL_IMM
%token DIV_IMM
%token AND_IMM
%token OR_IMM
%token SL_IMM
%token SR_IMM
%token ADD_X
%token SUB_X
%token MUL_X
%token DIV_X
%token AND_X
%token OR_X
%token SL_X
%token SR_X
%token NEG
%token JMP_IMM
%token LDX_IMM
%token JGT_IMM
%token JGE_IMM
%token JEQ_IMM
%token JAND_IMM
%token LD_LEN
%token LDX_LEN
%token JGT_X
%token JGE_X
%token JEQ_X
%token JAND_X

%token <imm> IMM OFFSET MEM_ADDR PKT_ADDR
%token NEWLINE EOF

%type <instr> pinstr

%type <solo_op> solo_op
%type <imm_op> imm_op
%type <br_op> br_op
%type <pkt_op> pkt_op
%type <mem_op> mem_op
%type <imm_br_op> imm_br_op

%start <list instr> pinstrs
%%

pinstrs:
    | pinstr=pinstr; NEWLINE; rest=pinstrs
      { (pinstr :: rest) }
    | pinstr=pinstr; EOF;
      { (pinstr :: []) }
    | EOF
      { [] }

pinstr: 
    | opcode=solo_op
      { SoloInstr opcode }
    | opcode=imm_op i=IMM
      { ImmInstr opcode i }
    | opcode=br_op b1=OFFSET b2=OFFSET
      { BrInstr opcode b1 b2 }
    | opcode=pkt_op po=PKT_ADDR
      { PktInstr opcode po }
    | opcode=mem_op mo=MEM_ADDR
      { MemInstr opcode mo }
    | opcode=imm_br_op i=IMM b1=OFFSET b2=OFFSET
      { ImmBrInstr opcode i b1 b2 }

pkt_op:
    | LD_WORD
      { LdWord }
    | LD_HALF
      { LdHalf }
    | LD_BYTE
      { LdByte }
    | LD_OFST_WORD
      { LdOfstWord }
    | LD_OFST_HALF
      { LdOfstHalf }
    | LDX_BYTE
      { LdXByte }

mem_op:
    | LD_MEM
      { LdMem }
    | LDX_MEM
      { LdXMem }
    | STORE
      { Store }
    | STORE_X
      { StoreX }

solo_op:
    | RET_A
      { RetA }
    | RET_K
      { RetK }
    | X_STORE_A
      { XStoreA }
    | A_STORE_X
      { AStoreX }
    | LDX_HDR_LEN
      { LdXHdrLen }
    | LD_LEN
      { LdLen }
    | LDX_LEN
      { LdXLen }

imm_op:
    | LD_IMM
      { LdImm }
    | ADD_IMM
      { AddImm }
    | SUB_IMM
      { SubImm }
    | MUL_IMM
      { MulImm }
    | DIV_IMM
      { DivImm }
    | AND_IMM
      { AndImm }
    | OR_IMM
      { OrImm }
    | SL_IMM
      { SLImm }
    | SR_IMM
      { SRImm }
    | ADD_X
      { AddX }
    | SUB_X
      { SubX }
    | MUL_X
      { MulX }
    | DIV_X
      { DivX }
    | AND_X
      { AndX }
    | OR_X
      { OrX }
    | SL_X
      { SLX }
    | SR_X
      { SRX }
    | NEG
      { Neg }
    | JMP_IMM
      { JmpImm }
    | LDX_IMM
      { LdXImm }

imm_br_op:
    | JGT_IMM
      { JGTImm }
    | JGE_IMM
      { JGEImm }
    | JEQ_IMM
      { JEqImm }
    | JAND_IMM
      { JAndImm }
  
br_op:
    | JGT_X
      { JGTX }
    | JGE_X
      { JGEX }
    | JEQ_X
      { JEqX }
    | JAND_X
      { JAndX }
