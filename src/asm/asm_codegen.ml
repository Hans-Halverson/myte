open Asm
open Asm_builders
open Asm_layout
open Mir
open Mir_type

let invalid_label_chars = Str.regexp "[-+<>,*():]"

let label_of_mir_label label = Str.global_replace invalid_label_chars "$" label

let get_asm_function_label ~(ir : Program.t) (func : Function.t) : label =
  if func == ir.main_func then
    main_label
  else if func.name == init_func_name then
    init_label
  else
    label_of_mir_label func.name

let mk_data_section () = Array.make 5 []

let add_data data init_data =
  let align_index = align_to_data_section_align_index (align_of_data_value init_data.value) in
  data.(align_index) <- init_data :: data.(align_index)

let add_bss bss uninit_data align =
  let align_index = align_to_data_section_align_index align in
  bss.(align_index) <- uninit_data :: bss.(align_index)

class globals_builder ~agg_cache =
  object (this)
    val mutable data : data = mk_data_section ()
    val mutable bss : bss = mk_data_section ()

    method data = data

    method bss = bss

    method add_data item = add_data data item

    method add_bss item align = add_bss bss item align

    method gen_global (global : Global.t) =
      let label = label_of_mir_label global.name in
      let add_immediate imm =
        let imm = cast_to_immediate imm in
        let size = bytes_of_size (size_of_immediate imm) in
        let is_pointer = is_pointer_type global.type_ in
        this#add_data { label; value = ImmediateData imm; size; is_pointer }
      in

      match global.init_val with
      (* Fake zero size global is not generated *)
      | _ when global.name = zero_size_name -> ()
      (* If uninitialized, place global variable in bss section *)
      | None ->
        let size = size_of_mir_type ~agg_cache global.type_ in
        let align = alignment_of_mir_type ~agg_cache global.type_ in
        let is_pointer = is_pointer_type global.type_ in
        this#add_bss { label; value = (); size; is_pointer } align
      (* Array literal is known at compile time, so insert into initialized data section *)
      | Some { value = { value = Lit (ArrayString data); _ }; _ } ->
        let size = String.length data in
        this#add_data { label; value = AsciiData data; size; is_pointer = false }
      | Some { value = { value = Lit (ArrayVtable (_, func_labels)); _ }; _ } ->
        let label_values =
          List.map
            (fun func_use ->
              let func = cast_to_function_literal func_use.Use.value in
              label_of_mir_label func.name)
            func_labels
        in
        let size = List.length label_values * pointer_size in
        this#add_data { label; value = LabelData label_values; size; is_pointer = false }
      (* Aggregate closure globals are special cased, with 0 set as environment *)
      | Some { value = { value = Lit (AggregateClosure (_, func_use)); _ }; _ } ->
        let func = cast_to_function_literal func_use.value in
        let func_data = LabelData [label_of_mir_label func.name] in
        let env_data = ImmediateData (Imm64 0L) in
        add_data
          data
          {
            label;
            value = ArrayData [func_data; env_data];
            size = ptr_size * 2;
            is_pointer = false;
          }
      (* Pointer and function literals are labels, so insert into initialized data section *)
      | Some { value = { value = Lit (Global { name = init_label; _ }); _ }; _ }
      | Some { value = { value = Lit (Function { name = init_label; _ }); _ }; _ } ->
        let init_label = label_of_mir_label init_label in
        let is_pointer = is_pointer_type global.type_ in
        this#add_data { label; value = LabelData [init_label]; size = pointer_size; is_pointer }
      (* Global is initialized to simple immediate, so insert into initialized data section *)
      | Some { value = { value = Lit (Bool b); _ }; _ } ->
        let value =
          if b then
            1
          else
            0
        in
        add_immediate (mk_imm ~imm:(Imm8 (Int8.of_int value)))
      | Some { value = { value = Lit (Byte b); _ }; _ } -> add_immediate (mk_imm ~imm:(Imm8 b))
      | Some { value = { value = Lit (Int i); _ }; _ } -> add_immediate (mk_imm ~imm:(Imm32 i))
      | Some { value = { value = Lit (Long l); _ }; _ } -> add_immediate (mk_imm ~imm:(Imm64 l))
      | Some { value = { value = Lit (Double d); _ }; _ } ->
        add_immediate (mk_imm ~imm:(Imm64 (Int64.bits_of_float d)))
      | Some { value = { value = Lit (NullPointer _); _ }; _ } ->
        add_immediate (mk_imm ~imm:(Imm64 0L))
      | Some { value = { value = Instr _ | Argument _; _ }; _ } ->
        failwith "Global initializer must be literal"
  end
