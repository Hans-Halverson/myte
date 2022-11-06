module FloatParts = struct
  type t = {
    is_positive: bool;
    exponent: int;
    mantissa_bits: Int64.t;
  }
end

let sign_mask = 0x8000000000000000L

let exponent_mask = 0x7FF0000000000000L

let mantissa_mask = 0x000FFFFFFFFFFFFFL

let get_exponent_of_bits (float_bits : Int64.t) : int =
  let exponent_bits = Int64.shift_right_logical (Int64.logand float_bits exponent_mask) 52 in
  Int64.to_int exponent_bits - 1023

let get_parts (n : Float.t) : FloatParts.t =
  let bits = Int64.bits_of_float n in
  let is_positive = Int64.equal (Int64.logand bits sign_mask) sign_mask in
  let exponent = get_exponent_of_bits bits in
  let mantissa_bits = Int64.logand bits mantissa_mask in

  { is_positive; exponent; mantissa_bits }

let is_positive_zero (f : Float.t) : bool = Int64.equal (Int64.bits_of_float f) Int64.zero
