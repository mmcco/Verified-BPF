open Array
open Sys
open Lex
open Lexing
open Parser
open Bpf
open Printf

let get_addr (rel_addr) =
    match rel_addr with
    | PktAddr addr -> addr
    | MemAddr addr -> addr

let get_opcode (instr) =
    match instr with
    | SoloInstr opcode ->
            Printf.sprintf "%s" opcode
    | ImmInstr (opcode, imm) ->
            Printf.sprintf "%s %d" opcode imm
    | OffsetInstr (opcode, offset) ->
            Printf.sprintf "%s %d" opcode (get_addr offset)
    | ImmBrInstr (opcode, imm, b1, b2) ->
            Printf.sprintf "%s %d %d %d" opcode imm (get_addr b1) (get_addr b2)
    | BrInstr (opcode, b1, b2) ->
            Printf.sprintf "%s %d %d" opcode (get_addr b1) (get_addr b2)
    | LenInstr opcode ->
            Printf.sprintf "%s" opcode

let rec fold_unit_list (u_list) =
    match u_list with
    | _ :: rest -> fold_unit_list rest
    | [] -> ()

let () =
    let cin =
      if Array.length Sys.argv > 1
      then open_in Sys.argv.(1)
      else stdin
    in
    let lexbuf = Lexing.from_channel cin in
    let pinstrs = Parser.pinstrs Lex.bpf_lex lexbuf in
    fold_unit_list (List.map print_endline (List.map get_opcode pinstrs))
