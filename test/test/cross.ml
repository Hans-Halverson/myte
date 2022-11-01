open Target

let parse_cross_target (cross_target_string : string) =
  if not (String.equal cross_target_string "") then
    let cross_target = Target.parse_target_string cross_target_string in
    if Target.equal !host cross_target then
      None
    else
      Some cross_target
  else
    None

let get_cross_run_prefix ~(host : Target.machine) ~(cross_target : Target.machine) =
  match (host.system, cross_target.system) with
  | (Linux, Linux) ->
    (match cross_target.architecture with
    | AArch64 -> "qemu-aarch64 -L /usr/aarch64-linux-gnu "
    | X86_64 -> "qemu-x86_64 -L /usr/x86_64-linux-gnu ")
  | _ ->
    failwith
      (Printf.sprintf
         "Cross testing for %s on %s not supported"
         (Target.target_triple cross_target)
         (Target.target_triple host))
