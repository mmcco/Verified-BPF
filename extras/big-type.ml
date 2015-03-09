type opcode =
    (*
     Loads to accumulator
    *)
    (* A <- P[k:4] *)
    | ld_word of int
    (* A <- P[k:2] *)
    | ld_half of int
    (* A <- P[k:1] *)
    | ld_byte of int
    (* A <- P[X+k:4] *)
    | ld_ofst_word of int
    (* A <- P[X+k:2] *)
    | ld_ofst_half of int
    (* A <- P[X+k:1] *)
    | ldx_byte of int
    (* A <- len *)
    | ld_len
    (* A <- k *)
    | ld_imm of int
    (* A <- M[k] *)
    | ld_mem of int

    (*
     Loads to index register
    *)
    (* X <- k *)
    | ldx_imm of int
    (* X <- M[k] *)
    | ldx_mem of int
    (* X <- len *)
    | ldx_len
    (* X <- 4*(P[k:1]&0xf) *)
    | ldx_hdr_len of int

    (* M[k] <- A *)
    | store of int

    (* M[k] <- X *)
    | store_x of int

    (* A <- A + k *)
    | add_imm of int
    (* A <- A - k *)
    | sub_imm of int
    (* A <- A * k *)
    | mul_imm of int
    (* A <- A / k *)
    | div_imm of int
    (* A <- A & k *)
    | and_imm of int
    (* A <- A | k *)
    | or_imm of int
    (* A <- A << k *)
    | sl_imm of int
    (* A <- A >> k *)
    | sr_imm of int
    (* A <- A + X *)
    | add_x
    (* A <- A - X *)
    | sub_x
    (* A <- A * X *)
    | mul_x
    (* A <- A / X *)
    | div_x
    (* A <- A & X *)
    | and_x
    (* A <- A | X *)
    | or_x
    (* A <- A << X *)
    | sl_x
    (* A <- A >> X *)
    | sr_x
    (* A <- -A *)
    | neg

    (* pc += k *)
    | jmp_imm of int
    (* pc += (A > k) ? jt : jf *)
    | jgt_imm of int * int * int
    (* pc += (A >= k) ? jt : jf *)
    | jge_imm of int * int * int
    (* pc += (A == k) ? jt : jf *)
    | jeq_imm of int * int * int
    (* pc += (A & k) ? jt : jf *)
    | jand_imm of int * int * int
    (* pc += (A > X) ? jt : jf *)
    | jgt_x of int * int
    (* pc += (A >= X) ? jt : jf *)
    | jge_x of int * int
    (* pc += (A == X) ? jt : jf *)
    | jeq_x of int * int
    (* pc += (A & X) ? jt : jf *)
    | jand_x of int * int

    (* Return, accept A bytes. *)
    | ret_a
    (* Return, accept k bytes. *)
    | ret_k

    (* X <- A *)
    | x_store_a
    (* A <- X *)
    | a_store_x;;
