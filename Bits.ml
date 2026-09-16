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

(* claude: no `type int64 = t` here -- unused/dead, no `Bits.int64` or
 * `int_of_bits64` counterpart was ever actually built, so that alias
 * was misleading (implying 64-bit support that doesn't exist). Add a
 * real `int_of_bits64 : t -> Int64.t` if something actually needs it. *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* claude: [size] can reach 32 (a field spanning the full word), where
 * `1 lsl size` would be an undefined shift on a 32-bit host's 31-bit
 * native int -- compares via Int64.t instead (always an exact,
 * lossless widening from a native int, any host), and skips the check
 * entirely for size = 32, since a full-word field can't overflow its
 * own space by definition. *)
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

(* claude: folds via Int32.t (always exactly 32 bits, any host)
 * instead of native-int `lsl`/`lor` -- `v lsl i` can reach bit 31
 * (instruction words routinely have their own top bit set: sign
 * bits, condition codes, opcode classes), which overflows a 32-bit
 * host's 31-bit native int. *)
let int_of_bits32 (xs : int32) : Int32.t =
  sanity_check_32 xs;
  xs |> List.fold_left (fun acc (v, i) ->
    Int32.logor (Int32.shift_left (Int32.of_int v) i) acc
  ) (Int32.of_int 0)
