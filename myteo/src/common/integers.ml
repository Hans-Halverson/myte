type base =
  | Dec
  | Bin
  | Hex

let min_unsigned_byte = 0L

let max_unsigned_byte = 256L

let min_signed_byte = -128L

let max_signed_byte = 127L

let min_unsigned_int = 0L

let max_unsigned_int = 4294967295L

let min_signed_int = -2147483648L

let max_signed_int = 2147483647L

let is_out_of_signed_byte_range x =
  Int64.compare min_signed_byte x = 1 || Int64.compare max_signed_byte x = -1

let is_out_of_signed_int_range x =
  Int64.compare min_signed_int x = 1 || Int64.compare max_signed_int x = -1

let is_out_of_unsigned_byte_range x =
  Int64.compare min_unsigned_byte x = 1 || Int64.compare max_unsigned_byte x = -1

let is_out_of_unsigned_int_range x =
  Int64.compare min_unsigned_int x = 1 || Int64.compare max_unsigned_int x = -1

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
