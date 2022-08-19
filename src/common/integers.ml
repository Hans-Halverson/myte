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

let trunc_int8_to_int1 x =
  let one = Int8.of_int 1 in
  Int8.equal one (Int8.logand x one)

let trunc_int32_to_int1 x = Int32.equal 1l (Int32.logand x 1l)

let trunc_int64_to_int1 x = Int64.equal 1L (Int64.logand x 1L)

let trunc_int64_to_int32 x =
  let bytes = Bytes.create 8 in
  Bytes.set_int64_le bytes 0 x;
  Bytes.get_int32_le bytes 0

let zext_int1_to_int8 x =
  Int8.of_int
    (if x then
      1
    else
      0)

let zext_int1_to_int32 x =
  if x then
    1l
  else
    0l

let zext_int1_to_int64 x =
  if x then
    1L
  else
    0L

let zext_int8_to_int32 x =
  let bytes = Bytes.make 4 '\x00' in
  Bytes.set_int8 bytes 0 (Int8.to_int x);
  Bytes.get_int32_le bytes 0

let zext_int8_to_int64 x =
  let bytes = Bytes.make 8 '\x00' in
  Bytes.set_int8 bytes 0 (Int8.to_int x);
  Bytes.get_int64_le bytes 0

let zext_int32_to_int64 x =
  let bytes = Bytes.make 8 '\x00' in
  Bytes.set_int32_le bytes 0 x;
  Bytes.get_int64_le bytes 0

let is_power_of_two x = Int64.equal 0L (Int64.logand x (Int64.sub x 1L))

(* Find rightmost bit that is set, index of bit minus 1 is power of two *)
let power_of_two x =
  let rec find_set_bit x i =
    if Int64.equal 1L x then
      i
    else
      find_set_bit (Int64.shift_right_logical x 1) (i + 1)
  in
  find_set_bit x 0

let int64_of_char char = Int64.of_int (int_of_char char)

let char_to_string char =
  if char = '"' then
    "\""
  else
    Char.escaped char
