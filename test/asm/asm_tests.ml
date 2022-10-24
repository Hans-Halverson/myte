open Myte_test

type run_type =
  | DumpAsmOnly
  (* Run all tests. Takes optional run command prefix, e.g. to add qemu *)
  | Full of string option

let get_arch_dir (arch : Target.architecture) =
  match arch with
  | AArch64 -> "aarch64"
  | X86_64 -> "x86_64"

let check_exit_code_commands =
  {|
    EXIT_CODE="$?";
    if [ "$EXIT_CODE" -ne 0 ]; then
      if [ "$EXIT_CODE" -eq 139 ]; then
        EXTRA_INFO=" (Segfault)"
      fi
      echo "NOTE: Exited with code $EXIT_CODE$EXTRA_INFO"
    fi
  |}

let commands ~config bin target run_type files =
  let files = String.concat " " files in
  let (dump_command, options) =
    match config with
    | Some "OPTIMIZE" -> ("--dump-asm", " -O")
    | Some "DUMP_BITMAPS" -> ("--dump-full-asm", " --custom-gc")
    | _ -> ("--dump-asm", "")
  in

  let common_options =
    let target_triple = Target.target_triple target in
    Printf.sprintf "--no-pretty-print --emit-all --target %s %s" target_triple options
  in

  let dump_asm_command = Printf.sprintf "%s %s %s %s" bin dump_command common_options files in

  let full_command =
    match run_type with
    | DumpAsmOnly -> Printf.sprintf "%s && echo '\nSKIP_REST'" dump_asm_command
    | Full run_command_prefix ->
      let run_command_prefix = Option.value ~default:"" run_command_prefix in
      Printf.sprintf
        {|
          %s && %s %s %s -o t.out;
          %s ./t.out;
          %s
          rm t.out 2> /dev/null
        |}
        dump_asm_command
        bin
        common_options
        files
        run_command_prefix
        check_exit_code_commands
  in

  [("test", full_command)]

let mk_snapshot_suite
    ~(bin : string) ~(record : bool) ~(target : Target.machine) ~(run_type : run_type) =
  let root = Sys.getcwd () in
  let relative_dir = Filename.dirname __FILE__ in
  let arch_dir = get_arch_dir target.architecture in
  let absolute_dir = Files.join_parts [root; relative_dir; arch_dir] in
  Snapshot_test.suite ~record absolute_dir (commands bin target run_type)

let suite ~bin ~record ~cross =
  let host = !Target.host in
  let mk_snapshot_suite = mk_snapshot_suite ~bin ~record in
  let cross_target =
    if not (String.equal cross "") then
      let cross_target = Target.parse_target_string cross in
      if Target.equal host cross_target then
        None
      else
        Some cross_target
    else
      None
  in
  let snapshot_suites =
    List.map
      (fun arch ->
        (* Run full tests only for host architecture *)
        if arch == host.architecture && cross_target == None then
          mk_snapshot_suite ~target:host ~run_type:(Full None)
        (* If cross target was specified, also run full tests for cross architecture *)
        else if cross_target != None && arch == (Option.get cross_target).architecture then
          let cross_target = Option.get cross_target in
          match (host.system, cross_target.system) with
          | (Linux, Linux) ->
            let run_command_prefix =
              match cross_target.architecture with
              | AArch64 -> "qemu-aarch64 -L /usr/aarch64-linux-gnu "
              | X86_64 -> "qemu-x86_64 -L /usr/x86_64-linux-gnu "
            in
            mk_snapshot_suite ~target:cross_target ~run_type:(Full (Some run_command_prefix))
          | _ ->
            failwith
              (Printf.sprintf
                 "Cross testing for %s on %s not supported"
                 (Target.target_triple cross_target)
                 (Target.target_triple host))
        (* Only dump assembly for other architectures *)
        else
          let linux_target = { Target.system = Linux; architecture = arch } in
          mk_snapshot_suite ~target:linux_target ~run_type:DumpAsmOnly)
      Target.all_architectures
  in
  { Suite.name = "asm"; suites = snapshot_suites @ [Liveness_analysis.suite]; tests = [] }
