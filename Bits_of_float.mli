(* ocaml-light's Int64/Int32 have no bits_of_float (nor float_of_bits) --
 * see this module's .ml for why and how this rebuilds the same bit
 * pattern from arithmetic instead. Host-width-independent: works the
 * same on a 32-bit host (i386, arm32, mips) as on a 64-bit one. *)

(* [f]'s 32-bit IEEE754 single-precision bit pattern (equivalent to
 * [Int32.bits_of_float f]). *)
val bits_of_float32: float -> Int32.t

(* [f]'s 64-bit IEEE754 double-precision bit pattern (equivalent to
 * [Int64.bits_of_float f]). *)
val bits_of_float64: float -> Int64.t

(* [f]'s 64-bit IEEE754 double-precision bit pattern split into its
 * high and low 32-bit halves (hi32, lo32), sign bit included in hi32. *)
val hi_lo_of_float64: float -> Int32.t * Int32.t
