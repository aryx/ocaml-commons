(* ocaml-light's Int64/Int32 have no bits_of_float (nor float_of_bits) --
 * see this module's .ml for why and how this rebuilds the same bit
 * pattern from arithmetic instead.
 *
 * Assumes a 64-bit host (63-bit native int) -- see the .ml. *)

(* The 32-bit IEEE754 single-precision bit pattern of [f], as an
 * unsigned int (equivalent to
 * [Int32.to_int (Int32.bits_of_float f) land 0xffffffff]). *)
val bits_of_float32: float -> int

(* The low 63 bits of [f]'s 64-bit IEEE754 double-precision bit
 * pattern, sign bit dropped (equivalent to
 * [Int64.to_int (Int64.bits_of_float f)]). *)
val bits_of_float64: float -> int

(* [f]'s 64-bit IEEE754 double-precision bit pattern split into its
 * high and low 32-bit halves (hi32, lo32), sign bit included in hi32. *)
val hi_lo_of_float64: float -> int * int
