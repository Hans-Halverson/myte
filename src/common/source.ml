type t =
  | File of string
  | String of string

let compare s1 s2 =
  match (s1, s2) with
  | (File f1, File f2) -> String.compare f1 f2
  | (String s1, String s2) -> String.compare s1 s2
  | (File _, String _) -> -1
  | (String _, File _) -> 1
