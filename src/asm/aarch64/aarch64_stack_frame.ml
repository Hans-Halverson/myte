open Aarch64_gen_context
open Asm
open Asm_builders
open Asm_instruction_definition
open Asm_register

let split_into_pairs list =
  let rec inner list acc =
    match list with
    | hd1 :: hd2 :: tl -> inner tl ([hd1; hd2] :: acc)
    | [hd] -> [hd] :: acc
    | [] -> acc
  in
  let rev_pairs = inner list [] in
  List.rev rev_pairs

let get_pair list =
  match list with
  | [hd1; hd2] -> (hd1, hd2)
  | _ -> failwith "Expected pair"

let mk_reg reg = mk_precolored ~type_:Long reg

let mk_sp () = mk_reg `SP

let write_function_prologue_and_epilogue (func : Function.t) =
  (* Instructions appended to prologue block, one at a time *)
  let prologue = func.prologue in
  let prologue_first_instr = get_first_instr_opt prologue in
  let add_prologue_instr instr operands =
    let instr = mk_blockless_instr instr operands in
    match prologue_first_instr with
    | None -> append_instruction prologue instr
    | Some first_instr -> insert_instruction_before ~before:first_instr instr
  in

  (* Find all ret instructions in function *)
  let epilogue_start_instrs = ref [] in
  func_iter_blocks func (fun block ->
      iter_instructions block (fun instr ->
          if instr.instr == `Ret then epilogue_start_instrs := ref instr :: !epilogue_start_instrs));

  (* Instructions prepended in epilogue sections, one at a time *)
  let add_epilogue_instr (f : unit -> Instruction.t) =
    List.iter
      (fun start_instr ->
        let new_instr = f () in
        insert_instruction_before ~before:!start_instr new_instr;
        start_instr := new_instr)
      !epilogue_start_instrs
  in

  (* Split callee saved registers into pairs so that load/store pair instructions can be used *)
  let callee_saved_reg_spills =
    RegSet.fold (fun acc reg -> acc :: reg) func.spilled_callee_saved_regs []
  in
  let callee_saved_reg_spills =
    if func.is_leaf then
      callee_saved_reg_spills
    else
      `R30 :: callee_saved_reg_spills
  in
  let callee_saved_reg_spill_pairs = split_into_pairs callee_saved_reg_spills in
  let reg_spill_size = List.length callee_saved_reg_spill_pairs * 16 in

  let add_spilled_reg_store_load reg_pair is_first offset =
    (* The very first load/store pair should update the stack pointer with pre/post index mode.
       All later load/store pairs use a postive offset from the already moved stack pointer. *)
    let (load_mode, store_mode, store_offset) =
      if is_first then
        (AArch64.PostIndex, AArch64.PreIndex, -offset)
      else
        (Offset, Offset, offset)
    in
    if List_utils.is_pair reg_pair then (
      let (reg2, reg1) = get_pair reg_pair in
      add_prologue_instr
        (`Stp (Size64, store_mode))
        [| mk_reg reg1; mk_reg reg2; mk_sp (); mk_imm16 ~n:store_offset |];
      add_epilogue_instr (fun _ ->
          mk_blockless_instr
            (`Ldp (Size64, load_mode))
            [| mk_reg reg1; mk_reg reg2; mk_sp (); mk_imm16 ~n:offset |])
    ) else
      let reg = List.hd reg_pair in
      add_prologue_instr
        (`StrI (X, store_mode))
        [| mk_reg reg; mk_sp (); mk_imm16 ~n:store_offset |];
      add_epilogue_instr (fun _ ->
          mk_blockless_instr
            (`LdrI (Size64, X, false, load_mode))
            [| mk_reg reg; mk_sp (); mk_imm16 ~n:offset |])
  in

  (* Write first load/store pair which updates stack pointer *)
  (match List_utils.hd_opt callee_saved_reg_spill_pairs with
  | None -> ()
  | Some reg_pair -> add_spilled_reg_store_load reg_pair true reg_spill_size);

  (* Write all remaining load store pairs *)
  List.iteri
    (fun i reg_pair ->
      let offset = (i + 1) * 16 in
      add_spilled_reg_store_load reg_pair false offset)
    (List_utils.tl_or_empty callee_saved_reg_spill_pairs)

let write_stack_frame ~(gcx : Gcx.t) =
  FunctionSet.iter (fun func -> write_function_prologue_and_epilogue func) gcx.funcs
