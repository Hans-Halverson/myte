type t = {
  check: bool ref;
  dump_ast: bool ref;
  dump_resolved_ast: bool ref;
  dump_ir: bool ref;
  dump_ir_transforms: string list ref;
  dump_virtual_asm: bool ref;
  dump_asm: bool ref;
  dump_debug: bool ref;
  dump_stdlib: bool ref;
  optimize: bool ref;
  output_file: string option ref;
  print_plain: bool ref;
  stdlib_path: string option ref;
}

let opts =
  {
    check = ref false;
    dump_ast = ref false;
    dump_resolved_ast = ref false;
    dump_ir = ref false;
    dump_ir_transforms = ref [];
    dump_virtual_asm = ref false;
    dump_asm = ref false;
    dump_debug = ref false;
    dump_stdlib = ref false;
    optimize = ref false;
    output_file = ref None;
    print_plain = ref false;
    stdlib_path = ref None;
  }

let split_comma_list string_list = String.split_on_char ',' string_list

let spec =
  [
    ("--check", Arg.Set opts.check, " Check program for errors but do not compile");
    ("--dump-ast", Arg.Set opts.dump_ast, " Print the AST to stdout");
    ("--dump-resolved-ast", Arg.Set opts.dump_resolved_ast, " Print the resolved AST to stdout");
    ("--dump-ir", Arg.Set opts.dump_ir, " Print the IR to stdout");
    ( "--dump-ir-transforms",
      Arg.String (fun transforms -> opts.dump_ir_transforms := split_comma_list transforms),
      " Transforms to apply to IR before printing to stdout" );
    ("--dump-virtual-asm", Arg.Set opts.dump_virtual_asm, " Print the virtual assembly to stdout");
    ("--dump-asm", Arg.Set opts.dump_asm, " Print the assembly to stdout");
    ("--dump-debug", Arg.Set opts.dump_debug, " Include debug info when printing other commands");
    ( "--dump-stdlib",
      Arg.Set opts.dump_stdlib,
      " Include the standard library when printing other commands" );
    ("--no-pretty-print", Arg.Set opts.print_plain, " Do not pretty print output");
    ("-o", Arg.String (fun file -> opts.output_file := Some file), " Write output to file");
    ("-O", Arg.Set opts.optimize, " Compile with optimizations");
    ("--stdlib", Arg.String (fun path -> opts.stdlib_path := Some path), " Path to standard library");
  ]
  |> Arg.align

let usage_message = "OVERVIEW: Myte programming language

USAGE: myte [options] file...

OPTIONS:"

let check () = !(opts.check)

let dump_ast () = !(opts.dump_ast)

let dump_resolved_ast () = !(opts.dump_resolved_ast)

let dump_ir () = !(opts.dump_ir)

let dump_ir_transforms () = !(opts.dump_ir_transforms)

let dump_virtual_asm () = !(opts.dump_virtual_asm)

let dump_asm () = !(opts.dump_asm)

let dump_debug () = !(opts.dump_debug)

let dump_stdlib () = !(opts.dump_stdlib)

let optimize () = !(opts.optimize)

let output_file () = !(opts.output_file)

let print_plain () = !(opts.print_plain)

let stdlib_path () = !(opts.stdlib_path)
