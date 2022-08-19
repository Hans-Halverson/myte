(* Integer in the range [-128, 127]. Represented by native integer, sign extended from int8 value. *)
type t = int

let to_int32 x = Int32.of_int x

let of_int32 x =
  let bytes = Bytes.create 4 in
  Bytes.set_int32_le bytes 0 x;
  Bytes.get_int8 bytes 0

let to_int64 x = Int64.of_int x

let of_int64 x =
  let bytes = Bytes.create 8 in
  Bytes.set_int64_le bytes 0 x;
  Bytes.get_int8 bytes 0

let to_int x = x

let of_int x = of_int64 (Int64.of_int x)

let to_float x = Float.of_int x

let to_string x = string_of_int x

let clamp_int_to_i8 x =
  let bytes = Bytes.create 8 in
  Bytes.set_int8 bytes 0 x;
  Bytes.get_int8 bytes 0

let neg x = clamp_int_to_i8 (-x)

let lognot x = clamp_int_to_i8 (lnot x)

let add x y = clamp_int_to_i8 (x + y)

let sub x y = clamp_int_to_i8 (x - y)

let mul x y = clamp_int_to_i8 (x * y)

let div x y = clamp_int_to_i8 (x / y)

let rem x y = clamp_int_to_i8 (x mod y)

let logand x y = clamp_int_to_i8 (x land y)

let logor x y = clamp_int_to_i8 (x lor y)

let logxor x y = clamp_int_to_i8 (x lxor y)

let shift_left x y = clamp_int_to_i8 (x lsl y)

let shift_right x y = clamp_int_to_i8 (x asr y)

let shift_right_logical x y = clamp_int_to_i8 ((x land 0xFF) lsr y)

let equal x y = Int.equal x y

let compare x y = Int.compare x y
