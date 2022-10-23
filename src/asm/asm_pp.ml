open Asm
open Asm_builders

type print_context = { mutable block_print_labels: string BlockMap.t }

let write_full_asm () =
  Opts.dump_full_asm () || ((not (Opts.dump_asm ())) && not (Opts.dump_virtual_asm ()))

let mk_pcx ~(funcs : FunctionSet.t) =
  let pcx = { block_print_labels = BlockMap.empty } in
  (* Find set of all blocks that have an incoming jump *)
  let incoming_jump_blocks = ref BlockSet.empty in
  funcs_iter_blocks funcs (fun block ->
      iter_instructions block (fun instr ->
          match instr with
          | { instr = `Jmp | `JmpCC _; operands = [| { value = Block next_block; _ } |]; _ } ->
            incoming_jump_blocks := BlockSet.add next_block !incoming_jump_blocks
          | _ -> ()));

  (* Determine labels for every unlabed block in program that has an incoming jump *)
  let max_label_id = ref 0 in
  FunctionSet.iter
    (fun func ->
      List.iter
        (fun block ->
          let open Block in
          match block.label with
          | Some _ -> ()
          | None ->
            if BlockSet.mem block !incoming_jump_blocks then (
              let id = !max_label_id in
              max_label_id := !max_label_id + 1;
              pcx.block_print_labels <-
                BlockMap.add block (".L" ^ string_of_int id) pcx.block_print_labels
            ))
        func.Function.blocks)
    funcs;
  pcx

(*
 * ============================
 *       Buffer Utilities
 * ============================
 *)

let mk_buf ?(size = 1024) () = Buffer.create size

let add_char ~buf c = Buffer.add_char buf c

let add_string ~buf str = Buffer.add_string buf str

let add_indent ~buf = Buffer.add_string buf "  "

let add_line ~buf ?(indent = true) f =
  if indent then add_indent ~buf;
  f buf;
  add_char ~buf '\n'

let add_blank_line ~buf = add_char ~buf '\n'

let add_label_line ~buf label =
  add_line ~buf ~indent:false (fun buf ->
      add_string ~buf label;
      add_char ~buf ':')

let quote_string str =
  let buf = mk_buf ~size:16 () in
  String.iter
    (fun c ->
      match c with
      | '"' -> add_string ~buf "\\\""
      | '\\' -> add_string ~buf "\\\\"
      | '\n' -> add_string ~buf "\\n"
      | '\t' -> add_string ~buf "\\t"
      | '\r' -> add_string ~buf "\\r"
      | c -> add_char ~buf c)
    str;
  "\"" ^ Buffer.contents buf ^ "\""

let pp_label_debug_prefix ~buf (block : Block.t) =
  if Opts.dump_debug () then (
    add_char ~buf '(';
    add_string ~buf (string_of_int block.id);
    add_string ~buf ") "
  )

let pp_label ~pcx block =
  let open Block in
  match block.label with
  | Some label -> Some label
  | None -> BlockMap.find_opt block pcx.block_print_labels

(*
 * ============================
 *        Data Sections
 * ============================
 *)

let rec pp_data_value ~buf (data_value : data_value) =
  let add_directive directive value =
    add_line ~buf (fun buf ->
        add_char ~buf '.';
        add_string ~buf directive;
        add_char ~buf ' ';
        add_string ~buf value)
  in
  let add_imm_directive imm =
    match imm with
    | Imm8 imm -> add_directive "byte" (Int8.to_string imm)
    | Imm16 imm -> add_directive "value" (string_of_int imm)
    | Imm32 imm -> add_directive "long" (Int32.to_string imm)
    | Imm64 imm -> add_directive "quad" (Int64.to_string imm)
  in
  match data_value with
  | ImmediateData imm -> add_imm_directive imm
  | AsciiData str -> add_directive "ascii" (quote_string str)
  | LabelData labels -> List.iter (fun label -> add_directive "quad" label) labels
  | ArrayData values -> List.iter (pp_data_value ~buf) values
  | SSELiteral imms -> List.iter add_imm_directive imms

let pp_initialized_data_item ~buf (init_data : initialized_data_item) =
  (* Data blocks have the form:
     label:
       .directive immediate *)
  add_label_line ~buf init_data.label;
  pp_data_value ~buf init_data.value

let pp_uninitialized_data_item ~buf (uninit_data : uninitialized_data_item) =
  add_label_line ~buf uninit_data.label;
  add_line ~buf (fun buf ->
      add_string ~buf ".skip ";
      add_string ~buf (string_of_int uninit_data.size))

let is_data_section_empty data_section = Array.for_all (fun data -> data = []) data_section

let pp_data_section ~buf data_section section_name pp_func =
  add_line ~buf (fun buf ->
      add_char ~buf '.';
      add_string ~buf section_name);
  Array.iteri
    (fun i data ->
      (* Align data between data values of differing alignments *)
      (if data <> [] && i <> 0 then
        let alignment = string_of_int (Int.shift_left 1 i) in
        add_line ~buf (fun buf ->
            add_string ~buf ".balign ";
            add_string ~buf alignment));

      if i = 3 && write_full_asm () then add_label_line ~buf (section_name ^ "_bitmap_start");

      if data <> [] then List.iter (pp_func ~buf) data)
    data_section

(* Add data sections *)
let pp_data_sections ~buf ~bss ~data =
  let write_full_asm = write_full_asm () in
  if write_full_asm || not (is_data_section_empty bss) then (
    add_blank_line ~buf;
    pp_data_section ~buf bss "bss" pp_uninitialized_data_item
  );

  if write_full_asm || not (is_data_section_empty data) then (
    add_blank_line ~buf;
    pp_data_section ~buf data "data" pp_initialized_data_item
  )

let pp_global_directive ~buf label = add_line ~buf (fun buf -> add_string ~buf (".global " ^ label))

(* Add all global directives *)
let pp_global_directives ~buf =
  pp_global_directive ~buf main_label;
  if write_full_asm () then (
    pp_global_directive ~buf init_label;
    pp_global_directive ~buf Std_lib.std_sys_init;

    pp_global_directive ~buf "bss_bitmap";
    pp_global_directive ~buf "bss_bitmap_size";
    pp_global_directive ~buf "bss_bitmap_start";

    pp_global_directive ~buf "data_bitmap";
    pp_global_directive ~buf "data_bitmap_size";
    pp_global_directive ~buf "data_bitmap_start"
  )

let pp_instr_debug_prefix ~buf ~(instr : Instruction.t) =
  if Opts.dump_debug () then (
    let instr_id = string_of_int instr.id in
    let padding_size = max 0 (4 - String.length instr_id) in
    add_char ~buf '#';
    add_string ~buf instr_id;
    add_string ~buf (String.make padding_size ' ')
  )
