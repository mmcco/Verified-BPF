%{
    open Lexing
    open Bpf
%}

%token <string> IMM_OP
%token <string> OFFSET_OP
%token <string> IMM_BR_OP
%token <string> BR_OP
%token <string> SOLO_OP
%token <string> LEN_OP

%token <int> IMM
%token <int> OFFSET
%token <int> MEM_ADDR
%token <int> PKT_ADDR

%token NEWLINE
%token EOF

%start <Bpf.instr list> pinstrs
%%

pinstrs:
    | pinstr = pinstr; NEWLINE; rest = pinstrs
      { (pinstr :: rest) }
    | pinstr = pinstr; EOF;
      { (pinstr :: []) }
    | EOF
      { [] }

pinstr: 
    | opcode=SOLO_OP
      { Bpf.SoloInstr (opcode) }
    | opcode=LEN_OP
      { Bpf.LenInstr (opcode) }
    | opcode=IMM_OP; imm=IMM
      { Bpf.ImmInstr (opcode, imm) }
    | opcode=OFFSET_OP; offset=PKT_ADDR
      { Bpf.OffsetInstr (opcode, Bpf.PktAddr (offset)) }
    | opcode=OFFSET_OP; offset=MEM_ADDR
      { Bpf.OffsetInstr (opcode, Bpf.MemAddr (offset)) }
    | opcode=IMM_BR_OP; imm=IMM; b1=OFFSET; b2=OFFSET
      { Bpf.ImmBrInstr (opcode, imm, b1, b2) }
    | opcode=BR_OP; b1=OFFSET; b2=OFFSET
      { Bpf.BrInstr (opcode, b1, b2) }
