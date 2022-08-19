type t

val to_int32 : t -> Int32.t

val of_int32 : Int32.t -> t

val to_int64 : t -> Int64.t

val of_int64 : Int64.t -> t

val to_int : t -> int

val of_int : int -> t

val to_float : t -> float

val to_string : t -> string

val neg : t -> t

val lognot : t -> t

val add : t -> t -> t

val sub : t -> t -> t

val mul : t -> t -> t

val div : t -> t -> t

val rem : t -> t -> t

val logand : t -> t -> t

val logor : t -> t -> t

val logxor : t -> t -> t

val shift_left : t -> int -> t

val shift_right : t -> int -> t

val shift_right_logical : t -> int -> t

val equal : t -> t -> bool

val compare : t -> t -> int
