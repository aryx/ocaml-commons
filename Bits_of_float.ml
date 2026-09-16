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
 * fields by hand.
 *
 * Used by the linker's various Codegen*/Datagen/Layout7 float-
 * literal-pool code, which needs the bit pattern (not the float
 * itself) to write into the object file's data section.
 *
 * claude: like Endian.ml's split_64, this assumes a 64-bit host: even
 * bits_of_float32 needs bit 31 settable (`sign lsl 31`), and
 * bits_of_float64/hi_lo_of_float64 need bit 62 (`hi32 lsl 32`) --
 * OCaml's native int only has that many magnitude bits on a 64-bit
 * host (63-bit int); on a 32-bit host (i386, arm32) it's only 31
 * bits, and both functions would silently produce garbage rather
 * than a clean error. Same assumption the amd64/arm64/riscv64
 * backends already make elsewhere (e.g. Endian.split_64's own
 * comment), not a new one. *)

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

(* claude: shared by both precisions -- [mantissa_bits]/[exp_bits] are
 * 23/8 for float32 and 52/11 for float64. Returns
 * (sign, biased_exponent, mantissa) for any float, special values
 * (0, -0, NaN, +/-infinity) included. *)
let decompose_bits (mantissa_bits : int) (exp_bits : int) (f : float) :
    int (* sign: 0 | 1 *) * int (* biased exponent *) * int (* mantissa *) =
  let bias = (1 lsl (exp_bits - 1)) - 1 in
  let max_biased_exp = (1 lsl exp_bits) - 1 in
  let sign = sign_bit_of_float f in
  if is_nan f then (0, max_biased_exp, 1 lsl (mantissa_bits - 1)) (* quiet NaN *)
  else if is_infinity f then (sign, max_biased_exp, 0)
  else
    let af = abs_float f in
    if af = 0.0 then (sign, 0, 0)
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
          int_of_float (floor (ldexp af (mantissa_bits - 1 + bias) +. 0.5)) in
        (sign, 0, mantissa)
      else if biased_exp >= max_biased_exp then
        (sign, max_biased_exp, 0) (* overflow to infinity *)
      else
        let frac = m *. 2.0 -. 1.0 in (* in [0.0, 1.0), the implicit leading 1 dropped *)
        let mantissa = int_of_float (floor (ldexp frac mantissa_bits +. 0.5)) in
        (* rounding frac up to 1.0 carries into the exponent *)
        if mantissa >= (1 lsl mantissa_bits)
        then (sign, biased_exp + 1, 0)
        else (sign, biased_exp, mantissa)

(* claude: matches `Int32.to_int (Int32.bits_of_float f) land 0xffffffff`
 * -- the full, correctly-signed 32-bit pattern as an unsigned int
 * (OCaml's 63-bit native int holds it exactly, no truncation). *)
let bits_of_float32 (f : float) : int =
  let (sign, biased_exp, mantissa) = decompose_bits 23 8 f in
  (sign lsl 31) lor (biased_exp lsl 23) lor mantissa

(* claude: matches `Int64.to_int (Int64.bits_of_float f)` -- i.e. the
 * *low 63 bits* of the true 64-bit pattern, sign bit dropped, same as
 * the real primitive would silently do once narrowed to OCaml's own
 * 63-bit native int (see Int64.to_int's own doc: "the high-order bit
 * is lost"). This is why the sign is ignored below: it's exactly the
 * bit that doesn't survive either way. Callers needing the true sign
 * of a negative double here already inherit that pre-existing
 * limitation (see Datagen.ml's own caveat about it). *)
let bits_of_float64 (f : float) : int =
  let (_sign, biased_exp, mantissa) = decompose_bits 52 11 f in
  let mantissa_hi = mantissa lsr 32 in
  let mantissa_lo = mantissa land 0xFFFFFFFF in
  let hi32 = (biased_exp lsl 20) lor mantissa_hi in
  (hi32 lsl 32) lor mantissa_lo

(* claude: unlike bits_of_float64 above, this keeps the sign bit --
 * each 32-bit half fits OCaml's 63-bit native int with no truncation,
 * so there's no reason to drop it here. Used by the archs (MIPS,
 * ARM64, RISC-V32) that materialize a double constant as two 32-bit
 * register loads instead of writing it into a data-section literal
 * pool (where bits_of_float64's truncated form is what the existing
 * byte-splitting helpers (array_64) expect). Returns (hi32, lo32). *)
let hi_lo_of_float64 (f : float) : int * int =
  let (sign, biased_exp, mantissa) = decompose_bits 52 11 f in
  let mantissa_hi = mantissa lsr 32 in
  let mantissa_lo = mantissa land 0xFFFFFFFF in
  let hi32 = (sign lsl 31) lor (biased_exp lsl 20) lor mantissa_hi in
  (hi32, mantissa_lo)
