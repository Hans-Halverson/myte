open X86_instructions

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

let pp_size_suffix ~buf size =
  add_char
    ~buf
    (match size with
    | Byte -> 'b'
    | Word -> 'w'
    | Long -> 'l'
    | Quad -> 'q')

let pp_register ~buf reg =
  add_char ~buf '%';
  add_string ~buf (string_of_int reg)

(* add_string
   ~buf
   (match reg with
   (* Quad registers *)
   | (A, Quad) -> "rax"
   | (B, Quad) -> "rbx"
   | (C, Quad) -> "rcx"
   | (D, Quad) -> "rdx"
   | (SI, Quad) -> "rsi"
   | (DI, Quad) -> "rdi"
   | (SP, Quad) -> "rsp"
   | (BP, Quad) -> "rbp"
   | (R8, Quad) -> "r8"
   | (R9, Quad) -> "r9"
   | (R10, Quad) -> "r10"
   | (R11, Quad) -> "r11"
   | (R12, Quad) -> "r12"
   | (R13, Quad) -> "r13"
   | (R14, Quad) -> "r14"
   | (R15, Quad) -> "r15"
   | (IP, _) -> "rip"
   (* Long registers *)
   | (A, Long) -> "eax"
   | (B, Long) -> "ebx"
   | (C, Long) -> "ecx"
   | (D, Long) -> "edx"
   | (SI, Long) -> "esi"
   | (DI, Long) -> "edi"
   | (SP, Long) -> "esp"
   | (BP, Long) -> "ebp"
   | (R8, Long) -> "r8d"
   | (R9, Long) -> "r9d"
   | (R10, Long) -> "r10d"
   | (R11, Long) -> "r11d"
   | (R12, Long) -> "r12d"
   | (R13, Long) -> "r13d"
   | (R14, Long) -> "r14d"
   | (R15, Long) -> "r15d"
   (* Word registers *)
   | (A, Word) -> "ax"
   | (B, Word) -> "bx"
   | (C, Word) -> "cx"
   | (D, Word) -> "dx"
   | (SI, Word) -> "si"
   | (DI, Word) -> "di"
   | (SP, Word) -> "sp"
   | (BP, Word) -> "bp"
   | (R8, Word) -> "r8w"
   | (R9, Word) -> "r9w"
   | (R10, Word) -> "r10w"
   | (R11, Word) -> "r11w"
   | (R12, Word) -> "r12w"
   | (R13, Word) -> "r13w"
   | (R14, Word) -> "r14w"
   | (R15, Word) -> "r15w"
   (* Byte registers *)
   | (A, Byte) -> "al"
   | (B, Byte) -> "bl"
   | (C, Byte) -> "cl"
   | (D, Byte) -> "dl"
   | (SI, Byte) -> "sil"
   | (DI, Byte) -> "dil"
   | (SP, Byte) -> "spl"
   | (BP, Byte) -> "bpl"
   | (R8, Byte) -> "r8b"
   | (R9, Byte) -> "r9b"
   | (R10, Byte) -> "r10b"
   | (R11, Byte) -> "r11b"
   | (R12, Byte) -> "r12b"
   | (R13, Byte) -> "r13b"
   | (R14, Byte) -> "r14b"
   | (R15, Byte) -> "r15b") *)

let pp_immediate ~buf imm =
  add_char ~buf '$';
  add_string
    ~buf
    (match imm with
    | ByteImmediate imm
    | WordImmediate imm ->
      string_of_int imm
    | LongImmediate imm -> Int32.to_string imm
    | QuadImmediate imm -> Int64.to_string imm)

let pp_memory_address ~buf mem =
  begin
    match mem.offset with
    | None -> ()
    | Some (ImmediateOffset imm) -> add_string ~buf (Int64.to_string imm)
    | Some (LabelOffset label) -> add_string ~buf label
  end;
  add_char ~buf '(';
  pp_register ~buf mem.base_register;
  begin
    match mem.index_and_scale with
    | None -> ()
    | Some (index_register, scale) ->
      add_string ~buf ", ";
      pp_register ~buf index_register;
      begin
        match scale with
        | Scale1 -> ()
        | Scale2 -> add_string ~buf ", 2"
        | Scale4 -> add_string ~buf ", 4"
        | Scale8 -> add_string ~buf ", 8"
      end
  end;
  add_char ~buf ')'

let pp_source ~buf source =
  match source with
  | ImmediateSource imm -> pp_immediate ~buf imm
  | RegisterSource reg -> pp_register ~buf reg
  | MemorySource mem -> pp_memory_address ~buf mem

let pp_destination ~buf dest =
  match dest with
  | RegisterDest reg -> pp_register ~buf reg
  | MemoryDest mem -> pp_memory_address ~buf mem

let pp_instruction ~buf instruction =
  add_line ~buf (fun buf ->
      let add_string = add_string ~buf in
      let pp_op op =
        add_string op;
        add_char ~buf ' '
      in
      let pp_source = pp_source ~buf in
      let pp_destination = pp_destination ~buf in
      let pp_register = pp_register ~buf in
      let pp_args_separator () = add_string ", " in
      match instruction with
      | Push source ->
        pp_op "push";
        pp_source source
      | Pop dest ->
        pp_op "dest";
        pp_destination dest
      | Mov (source, dest) ->
        pp_op "mov";
        pp_source source;
        pp_args_separator ();
        pp_destination dest
      | Lea (mem, reg) ->
        pp_op "lea";
        pp_memory_address ~buf mem;
        pp_args_separator ();
        pp_register reg
      (* Numeric operations *)
      | Neg dest ->
        pp_op "neg";
        pp_destination dest
      | Add (source, dest) ->
        pp_op "add";
        pp_source source;
        pp_args_separator ();
        pp_destination dest
      | Sub (source, dest) ->
        pp_op "sub";
        pp_source source;
        pp_args_separator ();
        pp_destination dest
      | IMul (source, dest) ->
        pp_op "imul";
        pp_source source;
        pp_args_separator ();
        pp_register dest
      | IDiv arg ->
        pp_op "idiv";
        pp_source arg
      (* Bitwise operations *)
      | Not dest ->
        pp_op "not";
        pp_destination dest
      | And (source, dest) ->
        pp_op "and";
        pp_source source;
        pp_args_separator ();
        pp_destination dest
      | Or (source, dest) ->
        pp_op "or";
        pp_source source;
        pp_args_separator ();
        pp_destination dest
      (* Comparisons *)
      | Cmp (arg1, arg2) ->
        pp_op "cmp";
        pp_source arg1;
        pp_args_separator ();
        pp_source arg2
      | Test (arg1, arg2) ->
        pp_op "test";
        pp_source arg1;
        pp_args_separator ();
        pp_source arg2
      | SetCmp (kind, reg) ->
        let op_str =
          match kind with
          | SetE -> "sete"
          | SetNE -> "setne"
          | SetL -> "setl"
          | SetLE -> "setle"
          | SetG -> "setg"
          | SetGE -> "setge"
        in
        pp_op op_str;
        pp_register reg
      (* Control flow *)
      | Jmp label ->
        pp_op "jmp";
        add_string label
      | CondJmp (cond_type, label) ->
        let op =
          match cond_type with
          | Equal -> "je"
          | NotEqual -> "jne"
          | LessThan -> "jl"
          | GreaterThan -> "jg"
          | LessThanEqual -> "jle"
          | GreaterThanEqual -> "jge"
        in
        pp_op op;
        add_string label
      | Call label ->
        pp_op "call";
        pp_source label
      | Leave -> add_string "leave"
      | Ret -> add_string "ret"
      | Syscall -> add_string "syscall")

let pp_data ~buf (data : data) =
  (* Data blocks have the form:
     label:
       .directive immediate *)
  add_label_line ~buf data.label;
  add_line ~buf (fun buf ->
      let (directive_string, value_string) =
        match data.value with
        | ImmediateData (ByteImmediate imm) -> ("byte", string_of_int imm)
        | ImmediateData (WordImmediate imm) -> ("value", string_of_int imm)
        | ImmediateData (LongImmediate imm) -> ("long", Int32.to_string imm)
        | ImmediateData (QuadImmediate imm) -> ("quad", Int64.to_string imm)
        | AsciiData str -> ("ascii", quote_string str)
      in
      add_char ~buf '.';
      add_string ~buf directive_string;
      add_char ~buf ' ';
      add_string ~buf value_string)

let pp_block ~buf (block : int block) =
  add_label_line ~buf block.label;
  List.iter (pp_instruction ~buf) block.instructions

let pp_x86_executable executable =
  let buf = mk_buf () in
  (* Add global directive *)
  add_line ~buf (fun buf -> add_string ~buf ".global _main");
  if executable.bss <> [] then
    List.iter
      (fun { label; size } ->
        add_line ~buf (fun buf -> add_string ~buf (Printf.sprintf ".lcomm %s, %d" label size)))
      executable.bss;
  (* Add rodata section *)
  if executable.rodata <> [] then (
    add_blank_line ~buf;
    add_line ~buf (fun buf -> add_string ~buf ".section \"r\", rodata");
    List.iter (pp_data ~buf) executable.rodata
  );
  (* Add data section *)
  if executable.data <> [] then (
    add_blank_line ~buf;
    add_line ~buf (fun buf -> add_string ~buf ".data");
    List.iter (pp_data ~buf) executable.data
  );
  (* Add text section *)
  add_blank_line ~buf;
  add_line ~buf (fun buf -> add_string ~buf ".text");
  List.iter (pp_block ~buf) executable.text;
  Buffer.contents buf
