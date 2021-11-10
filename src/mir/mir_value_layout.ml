module rec ValueLayout : sig
  type t =
    | Integer
    | Pointer of BlockLayout.t
end =
  ValueLayout

and BlockLayout : sig
  type t = {
    (* 8 bit tag *)
    tag: int;
    (* Values in memory block, of size up to 2^54. *)
    values: ValueLayout.t list;
  }
end =
  BlockLayout

let long_value_of_int i = `LongL (Int64.add 1L (Int64.shift_left (Int64.of_int i) 1))

let true_value = `LongL 3L

let false_value = `LongL 1L

let long_value_of_bool b =
  if b then
    true_value
  else
    false_value

let opaque_bits_tag = 255

let long_value_layout = { BlockLayout.tag = opaque_bits_tag; values = [Integer] }

let header_value_of_block_layout layout =
  let { BlockLayout.tag; values } = layout in
  let size = Int64.of_int (List.length values) in
  let shifted_size = Int64.shift_left size 10 in
  `LongL (Int64.add (Int64.of_int tag) shifted_size)
