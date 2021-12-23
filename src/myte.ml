open Basic_collections

let () =
  let files = ref SSet.empty in
  Arg.parse Opts.spec (fun file -> files := SSet.add file !files) Opts.usage_message;
  if SSet.is_empty !files then (
    Arg.usage Opts.spec Opts.usage_message;
    exit 1
  );
  Target.detect ();
  Installation.detect ();
  Driver_myte.compile !files
