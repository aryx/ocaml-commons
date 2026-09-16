(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2.1 as published by the Free Software Foundation,
 * with the special exception on linking described in file
 * license.txt.
 *)

(* claude: the "official" way to get a float's raw IEEE754 bit
 * pattern in real OCaml is a single call to Int64.bits_of_float (or
 * Int32.bits_of_float for single precision) -- both are externals
 * backed by a C primitive that just reinterprets the in-memory
 * double/single as an integer, no actual computation involved:
 *   external bits_of_float : float -> int64 = "caml_int64_bits_of_float"
 *
 * ocaml-light's own Int64/Int32 modules were never given that
 * primitive (see their .mli: only arithmetic externals -- add, mul,
 * shift, logand, etc. -- no bit-reinterpretation, and no
 * float_of_bits either), so `Int64.bits_of_float`/
 * `Int32.bits_of_float` are simply unbound values there. This module
 * rebuilds the same bit pattern from scratch using only arithmetic
 * that *is* available everywhere: decompose the float into
 * sign/exponent/mantissa via the standard `frexp`/`ldexp` (real
 * primitives, present in ocaml-light too) and reassemble the IEEE754
 * fields by hand -- entirely through Int32.t/Int64.t arithmetic, so
 * (unlike an earlier version of this module) nothing here assumes a
 * particular host word size: it works the same on a 64-bit host and
 * on a 32-bit one (i386, arm32, mips). Only `int_of_float`/`of_int`
 * on values already known to fit in a byte (0..255) ever touch the
 * native, host-width-dependent `int` type.
 *
 * Used by the linker's various Codegen*/Datagen/Layout7 float-
 * literal-pool code, which needs the bit pattern (not the float
 * itself) to write into the object file's data section. Callers
 * narrowing the Int32.t/Int64.t result down to a plain `int` (e.g. to
 * feed Endian.array_32/array_64) re-inherit whatever host-width
 * assumptions those callers already made on their own -- not this
 * module's concern. *)

(* claude: `x <> x` is the classic NaN test: it's the only float for
 * which self-equality fails. *)
let is_nan (f : float) : bool = f <> f

(* claude: `x +. x = x` holds only for x = 0.0 or x = +/-infinity (2x
 * = x has no other IEEE754 solution) -- avoids needing a pre-existing
 * `infinity` constant, which this ocaml-light Pervasives doesn't
 * define either. *)
let is_infinity (f : float) : bool =
  (not (is_nan f)) && f <> 0.0 && f +. f = f

(* claude: plain `f < 0.0` can't tell -0.0 from 0.0 (IEEE754 defines
 * them equal), so zero needs its own test via division. *)
let sign_bit_of_float (f : float) : int =
  if f = 0.0
  then (if 1.0 /. f < 0.0 then 1 else 0)
  else (if f < 0.0 then 1 else 0)

(* claude: [f] must be an exact, non-negative integer value (as a
 * float) below 2^63 -- true of every mantissa this module ever builds
 * (at most 52 bits, and doubles represent integers exactly up to
 * 2^53). Extracts it into an Int64.t one byte at a time via
 * mod_float/floor, so the native `int` this passes through
 * (`int_of_float` of a value always in 0..255) never needs to be
 * wider than a byte -- safe on a 32-bit host too. *)
let int64_of_nonneg_float (f : float) : Int64.t =
  let rec loop (remaining : float) (shift : int) (acc : Int64.t) : Int64.t =
    if remaining = 0.0 then acc
    else
      let byte = int_of_float (mod_float remaining 256.0) in
      loop (floor (remaining /. 256.0)) (shift + 8)
        (Int64.logor acc (Int64.shift_left (Int64.of_int byte) shift))
  in
  loop f 0 (Int64.of_int 0)

(* claude: shared by both precisions -- [mantissa_bits]/[exp_bits] are
 * 23/8 for float32 and 52/11 for float64. Returns
 * (sign, biased_exponent, mantissa) for any float, special values
 * (0, -0, NaN, +/-infinity) included. The mantissa is always an
 * Int64.t (wide enough for float64's 52 bits); float32's own 23-bit
 * mantissa just leaves the high bits at 0. *)
let decompose_bits (mantissa_bits : int) (exp_bits : int) (f : float) :
    int (* sign: 0 | 1 *) * int (* biased exponent *) * Int64.t (* mantissa *) =
  let bias = (1 lsl (exp_bits - 1)) - 1 in
  let max_biased_exp = (1 lsl exp_bits) - 1 in
  let sign = sign_bit_of_float f in
  if is_nan f
  then (0, max_biased_exp, Int64.shift_left (Int64.of_int 1) (mantissa_bits - 1)) (* quiet NaN *)
  else if is_infinity f then (sign, max_biased_exp, Int64.of_int 0)
  else
    let af = abs_float f in
    if af = 0.0 then (sign, 0, Int64.of_int 0)
    else
      (* af = m *. 2.0 ** (float e), with 0.5 <= m < 1.0 *)
      let (m, e) = frexp af in
      let unbiased_exp = e - 1 in
      let biased_exp = unbiased_exp + bias in
      if biased_exp <= 0 then
        (* subnormal (or underflows all the way to 0) -- no implicit
         * leading 1, so scale af directly by the smallest subnormal's
         * own exponent instead of extracting a fractional mantissa. *)
        let mantissa =
          int64_of_nonneg_float (floor (ldexp af (mantissa_bits - 1 + bias) +. 0.5)) in
        (sign, 0, mantissa)
      else if biased_exp >= max_biased_exp then
        (sign, max_biased_exp, Int64.of_int 0) (* overflow to infinity *)
      else
        let frac = m *. 2.0 -. 1.0 in (* in [0.0, 1.0), the implicit leading 1 dropped *)
        let mantissa_f = floor (ldexp frac mantissa_bits +. 0.5) in
        (* rounding frac up to 1.0 carries into the exponent -- compared
         * as floats (against 2.0 ** mantissa_bits) rather than after
         * converting to Int64.t, so this needs no extra width check. *)
        if mantissa_f >= ldexp 1.0 mantissa_bits
        then (sign, biased_exp + 1, Int64.of_int 0)
        else (sign, biased_exp, int64_of_nonneg_float mantissa_f)

(* claude: matches `Int32.bits_of_float f` -- the full, correctly-signed
 * 32-bit pattern. *)
let bits_of_float32 (f : float) : Int32.t =
  let (sign, biased_exp, mantissa) = decompose_bits 23 8 f in
  let sign32 = Int32.of_int sign in
  let biased_exp32 = Int32.of_int biased_exp in
  let mantissa32 = Int64.to_int32 mantissa in
  Int32.logor (Int32.shift_left sign32 31)
    (Int32.logor (Int32.shift_left biased_exp32 23) mantissa32)

(* claude: matches `Int64.bits_of_float f` -- the full, correctly-signed
 * 64-bit pattern (unlike an earlier version of this module, the sign
 * bit is no longer dropped: that was only needed to fit the result in
 * a 63-bit native `int`, a constraint this Int64.t-returning version
 * doesn't have). *)
let bits_of_float64 (f : float) : Int64.t =
  let (sign, biased_exp, mantissa) = decompose_bits 52 11 f in
  let sign64 = Int64.of_int sign in
  let biased_exp64 = Int64.of_int biased_exp in
  Int64.logor (Int64.shift_left sign64 63)
    (Int64.logor (Int64.shift_left biased_exp64 52) mantissa)

(* claude: [f]'s 64-bit pattern split into its high/low 32-bit halves,
 * sign bit included in the high half. Used by the archs (MIPS, ARM64,
 * RISC-V32) that materialize a double constant as two 32-bit register
 * loads instead of writing it into a data-section literal pool. *)
let hi_lo_of_float64 (f : float) : Int32.t * Int32.t =
  let bits = bits_of_float64 f in
  let hi = Int64.to_int32 (Int64.shift_right_logical bits 32) in
  let lo = Int64.to_int32 bits in
  (hi, lo)
