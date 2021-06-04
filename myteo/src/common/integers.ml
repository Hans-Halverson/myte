type base =
  | Dec
  | Bin
  | Hex

let max_byte = Int64.of_int 127

let min_byte = Int64.of_int (-128)

let max_int = Int64.of_int32 Int32.max_int

let min_int = Int64.of_int32 Int32.min_int
