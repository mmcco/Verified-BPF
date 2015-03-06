type rel_addr =
    | PktAddr of int
    | MemAddr of int
;;

type instr =
    | SoloInstr of string
    | ImmInstr of string * int
    | OffsetInstr of string * rel_addr
    | ImmBrInstr of string * int * int * int
    | BrInstr of string * int * int
    | LenInstr of string
;;
