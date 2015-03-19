%{

Require Import String.

Definition imm : Type := nat % type.
Definition pkt_ofst : Type := nat % type.
Definition mem_ofst : Type := nat % type.
Definition offset : Type := nat % type.

Inductive instr :=
    | SoloOp : instr
    | ImmOp : token -> imm -> instr
    | MemOp : token -> mem_ofst -> instr
    | PktOp : token -> pkt_ofst -> instr
    | BrOp : offset -> offset -> instr
    | ImmBrOp : imm -> offset -> offset -> instr.

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
%type <token> solo_op
%type <token> imm_op
%type <token> br_op
%type <token> pkt_op
%type <token> mem_op
%type <token> imm_br_op

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
      { SoloOp opcode }
    | opcode=imm_op i=IMM
      { ImmOp opcode i }
    | opcode=br_op b1=OFFSET b2=OFFSET
      { BrOp opcode b1 b2 }
    | opcode=pkt_op po=PKT_ADDR
      { PktOp po }
    | opcode=mem_op mo=MEM_ADDR
      { MemOp mo }
    | opcode=imm_br_op i=IMM b1=OFFSET b2=OFFSET
      { ImmBrOp i b1 b2 }

pkt_op:
    | LD_WORD
      { LD_WORD }
    | LD_HALF
      { LD_HALF }
    | LD_BYTE
      { LD_BYTE }
    | LD_OFST_WORD
      { LD_OFST_WORD }
    | LD_OFST_HALF
      { LD_OFST_HALF }
    | LDX_BYTE
      { LDX_BYTE }

mem_op:
    | LD_MEM
      { LD_MEM }
    | LDX_MEM
      { LDX_MEM }
    | STORE
      { STORE }
    | STORE_X
      { STORE_X }

solo_op:
    | RET_A
      { RET_A }
    | RET_K
      { RET_K }
    | X_STORE_A
      { X_STORE_A }
    | A_STORE_X
      { A_STORE_X }
    | LDX_HDR_LEN
      { LDX_HDR_LEN }
    | LD_LEN
      { LD_LEN }
    | LDX_LEN
      { LDX_LEN }

imm_op:
    | LD_IMM
      { LD_IMM }
    | ADD_IMM
      { ADD_IMM }
    | SUB_IMM
      { SUB_IMM }
    | MUL_IMM
      { MUL_IMM }
    | DIV_IMM
      { DIV_IMM }
    | AND_IMM
      { AND_IMM }
    | OR_IMM
      { OR_IMM }
    | SL_IMM
      { SL_IMM }
    | SR_IMM
      { SR_IMM }
    | ADD_X
      { ADD_X }
    | SUB_X
      { SUB_X }
    | MUL_X
      { MUL_X }
    | DIV_X
      { DIV_X }
    | AND_X
      { AND_X }
    | OR_X
      { OR_X }
    | SL_X
      { SL_X }
    | SR_X
      { SR_X }
    | NEG
      { NEG }
    | JMP_IMM
      { JMP_IMM }
    | LDX_IMM
      { LDX_IMM }

imm_br_op:
    | JGT_IMM
      { JGT_IMM }
    | JGE_IMM
      { JGE_IMM }
    | JEQ_IMM
      { JEQ_IMM }
    | JAND_IMM
      { JAND_IMM }
  

br_op:
    | JGT_X
      { JGT_X }
    | JGE_X
      { JGE_X }
    | JEQ_X
      { JEQ_X }
    | JAND_X
      { JAND_X }
