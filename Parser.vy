%{
    Require Import String.
    Require Import Integers.
    Require Import Bpf.
%}

%token <string> IMM_OP
%token <string> OFFSET_OP
%token <string> IMM_BR_OP
%token <string> BR_OP
%token <string> SOLO_OP
%token <string> LEN_OP

%token <nat> IMM
%token <nat> OFFSET
%token <nat> MEM_ADDR
%token <nat> PKT_ADDR

%token NEWLINE
%token EOF

%type <instr> pinstr

%start <list instr> pinstrs
%%

pinstrs:
    | pinstr = pinstr; NEWLINE; rest = pinstrs
      { (pinstr :: rest) }
    | pinstr = pinstr; EOF;
      { (pinstr :: []) }
    | EOF
      { [] }

pinstr: 
    | opcode = SOLO_OP
      { Bpf.solo_instr opcode }
    | opcode = LEN_OP
      { Bpf.len_instr opcode }
    | opcode = IMM_OP imm = IMM
      { Bpf.imm_instr opcode imm }
    | opcode = OFFSET_OP offset = PKT_ADDR
      { Bpf.offset_instr opcode (Bpf.pkt_addr offset) }
    | opcode = OFFSET_OP offset = MEM_ADDR
      { Bpf.offset_instr opcode (Bpf.mem_addr offset) }
    | opcode = IMM_BR_OP imm = IMM b1 = OFFSET b2 = OFFSET
      { Bpf.imm_br_instr opcode imm b1 b2 }
    | opcode = BR_OP b1 = OFFSET b2 = OFFSET
      { Bpf.br_instr opcode b1 b2 }
