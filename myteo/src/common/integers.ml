type base =
  | Dec
  | Bin
  | Hex

let max_byte = Int64.of_int 127

let min_byte = Int64.of_int (-128)

let max_int = Int64.of_int32 Int32.max_int

let min_int = Int64.of_int32 Int32.min_int

let is_out_of_byte_range x = Int64.compare min_byte x = 1 || Int64.compare max_byte x = -1

let is_out_of_int_range x = Int64.compare min_int x = 1 || Int64.compare max_int x = -1

let int64_of_string_opt raw base =
  match Int64.of_string_opt raw with
  | None -> None
  | Some value ->
    (match base with
    | Dec -> Some value
    | Hex
    | Bin ->
      (* OCaml's Int64.of_string treats hex and bin literals as unsigned. Check for overflow by
         checking for the existance of a minus sign in the raw string vs whether the parsed
         value is negative (excluding 0). *)
      let should_be_negative = raw.[0] = '-' in
      let value_is_negative = Int64.compare value Int64.zero = -1 in
      if value <> Int64.zero && value_is_negative <> should_be_negative then
        None
      else
        Some value)
