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

let int64_of_char char = Int64.of_int (int_of_char char)

let int64_less_than x y = Int64.compare x y == -1

let char_to_string char =
  if char = '"' then
    "\""
  else
    Char.escaped char

(* Find number of trailing zeros (starting from the lowest bit) using de Bruijn indices:
   http://supertech.csail.mit.edu/papers/debruijn.pdf *)

let debruijn_ctz32_table = [|
   0;  1; 28;  2; 29; 14; 24; 3;
  30; 22; 20; 15; 25; 17;  4; 8;
  31; 27; 13; 23; 21; 19; 16; 7;
  26; 12; 18;  6; 11;  5; 10; 9
|] [@@ocamlformat "disable"]

let debruijn_ctz64_table = [|
   0;  1; 48;  2; 57; 49; 28;  3;
  61; 58; 50; 42; 38; 29; 17;  4;
  62; 55; 59; 36; 53; 51; 43; 22;
  45; 39; 33; 30; 24; 18; 12;  5;
  63; 47; 56; 27; 60; 41; 37; 16;
  54; 35; 52; 21; 44; 32; 23; 11;
  46; 26; 40; 15; 34; 20; 31; 10;
  25; 14; 19;  9; 13;  8;  7;  6
|] [@@ocamlformat "disable"]

let int32_ctz (x : Int32.t) =
  if Int32.equal x 0l then
    32
  else
    let index =
      Int32.shift_right_logical (Int32.mul (Int32.logand x (Int32.neg x)) 0x077CB531l) 27
    in
    debruijn_ctz32_table.(Int32.to_int index)

let int64_ctz (x : Int64.t) =
  if Int64.equal x 0L then
    64
  else
    let index =
      Int64.shift_right_logical (Int64.mul (Int64.logand x (Int64.neg x)) 0x03f79d71b4cb0a89L) 58
    in
    debruijn_ctz64_table.(Int64.to_int index)

(* Find number of leading zeros (starting from the highest bit) by first finding which byte
   contains the highest one by binary searching by bytes. Then look up the byte in a lookup table
   to find the (inverse of the) number of leading zeros in that byte. *)

let byte_clz_table = [|
  0; 1; 2; 2; 3; 3; 3; 3; 4; 4; 4; 4; 4; 4; 4; 4;
  5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5; 5;
  6; 6; 6; 6; 6; 6; 6; 6; 6; 6; 6; 6; 6; 6; 6; 6;
  6; 6; 6; 6; 6; 6; 6; 6; 6; 6; 6; 6; 6; 6; 6; 6;
  7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7;
  7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7;
  7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7;
  7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7; 7;
  8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8;
  8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8;
  8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8;
  8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8;
  8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8;
  8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8;
  8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8;
  8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8; 8
|] [@@ocamlformat "disable"]

let int32_clz (n32 : Int32.t) =
  let (n16, offset) =
    if Int32.unsigned_compare n32 0xFFFFl == 1 then
      (Int32.shift_right_logical n32 16, 16)
    else
      (n32, 0)
  in
  let (n8, offset) =
    if Int32.unsigned_compare n16 0xFFl == 1 then
      (Int32.shift_right_logical n16 8, offset + 8)
    else
      (n16, offset)
  in
  32 - (offset + byte_clz_table.(Int32.to_int n8))

let int64_clz (n64 : Int64.t) =
  let (n32, offset) =
    if Int64.unsigned_compare n64 0xFFFFFFFFL == 1 then
      (Int64.shift_right_logical n64 32, 32)
    else
      (n64, 0)
  in
  let (n16, offset) =
    if Int64.unsigned_compare n32 0xFFFFL == 1 then
      (Int64.shift_right_logical n32 16, offset + 16)
    else
      (n32, offset)
  in
  let (n8, offset) =
    if Int64.unsigned_compare n16 0xFFL == 1 then
      (Int64.shift_right_logical n16 8, offset + 8)
    else
      (n16, offset)
  in
  64 - (offset + byte_clz_table.(Int64.to_int n8))

(* Rotate an integer left by the specified number of bits. Rotate left by passing a negative
   rotate argument. *)

let int32_rotate_left (x : Int32.t) (n : int) =
  let n_mod_31 = n land 31 in
  let high_bits = Int32.shift_left x n_mod_31 in
  let low_bits = Int32.shift_right_logical x (32 - n_mod_31) in
  Int32.logor high_bits low_bits

let int64_rotate_left (x : Int64.t) (n : int) =
  let n_mod_64 = n land 63 in
  let high_bits = Int64.shift_left x n_mod_64 in
  let low_bits = Int64.shift_right_logical x (64 - n_mod_64) in
  Int64.logor high_bits low_bits
