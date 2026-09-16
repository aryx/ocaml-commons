
type t = (int * int) list
[@@deriving show]

type int32 = t

val int_of_bits32: int32 -> Int32.t
