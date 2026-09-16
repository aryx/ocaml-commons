(* Yoann Padioleau
 *
 * Copyright (C) 2025 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Declarative bits building.
 *
 * alt:
 *  - use bit bool ops and bit shifting ops at build time, like in C,
 *    but more tedious in OCaml than C and anyway lose some check opportunity
 *  - use struct bitfields like in C? some bitfield libs for that in OCaml?
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* More declarative way to build integers from bits and give opportunity to
 * sanity check if overlap.
 * TODO: (int * intsized) list; and intsized = I1 of int | I2 of int | I3 of int
 * to store number of bits used and extend sanity check to check for
 * overflow?
*)
type t = (int * int) list
[@@deriving show]

type int32 = t

(* claude: there used to also be a `type int64 = t` here, but it was
 * dead: grepping the whole repo, only `Bits.int32`/`int_of_bits32` are
 * ever used -- no `Bits.int64` or an `int_of_bits64` counterpart was
 * ever actually built. An alias that implies 64-bit support which
 * doesn't exist is worse than no alias at all, so it's just removed;
 * add a real `int_of_bits64 : t -> Int64.t` (same shape as
 * int_of_bits32 below) if/when something actually needs it. *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* claude: [size]'s upper bound (32, from [aux]'s own starting `bit`)
 * means `1 lsl size` here used to be able to reach `1 lsl 32`, one
 * more than OCaml's native int can shift by meaningfully on a 32-bit
 * host (31-bit int) -- not just a magnitude problem like
 * int_of_bits32's own fold below, but genuinely undefined-shift
 * territory. Compares via Int64.t instead (Int64.of_int on an
 * already-nonneg native int -- checked by the `x < 0` case right
 * above -- is always an exact, lossless widening, regardless of host
 * word size) so the check itself can never misbehave; a field that
 * spans the full 32 bits (size = 32) trivially can't overflow its own
 * space, so that case is skipped rather than shifted by 32. *)
let sanity_check_32 (xs : t) : unit =
  let dbg = Dumper.dump xs in
  let rec aux bit xs =
    match xs with
    | [] -> ()
    | (x,bit2)::xs ->
        let size = bit - bit2 in
        (match () with
        | _ when bit2 >= bit -> failwith ("composed word not sorted: " ^ dbg)
        | _ when x < 0       -> failwith (spf "negative value %d: %s" x dbg)
        | _ when size <= 0   -> failwith (spf "no space for value %d: %s" x dbg)
        (* if size = 2 then maxval = 3 so x >= 2^2 (= 1 lsl 2) then error *)
        | _ when size < 32 &&
                 Int64.of_int x >= Int64.shift_left (Int64.of_int 1) size ->
            failwith (spf "value %d overflow outside its space (%d - %d): %s "
                       x bit bit2 dbg)
        | _ -> ()
        );
        aux bit2 xs
  in
  aux 32 xs

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* claude: recent OCaml would just do:
 *   let int_of_bits32 (xs : int32) : int =
 *     sanity_check_32 xs;
 *     xs |> List.fold_left (fun acc (v, i) -> (v lsl i) lor acc) 0
 * -- `v lsl i` can reach bit 31 (instruction words routinely have
 * their own top bit set: sign bits, condition codes, opcode classes),
 * which overflows a 32-bit host's 31-bit native int. Folds via
 * Int32.t instead, which is always exactly 32 bits regardless of host
 * word size. Individual field values ([v] above) are still assumed to
 * already fit in a native int on their own -- that's the caller's own
 * concern (e.g. a still-native-int `Types.word`), not something this
 * function's own fold can make worse or better. *)
let int_of_bits32 (xs : int32) : Int32.t =
  sanity_check_32 xs;
  xs |> List.fold_left (fun acc (v, i) ->
    Int32.logor (Int32.shift_left (Int32.of_int v) i) acc
  ) (Int32.of_int 0)
