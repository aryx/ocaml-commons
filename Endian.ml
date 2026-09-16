open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Little vs Big Endian input/output.
 * See https://en.wikipedia.org/wiki/Endianness
 *
 * Note that OCaml 4.08 introduced many endian-related functions in buffer.mli,
 * bytes.mli, and string.mli. See also Sys.big_endian bool since 4.00.0
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* claude: takes an Int32.t (always exactly 32 bits, any host) rather
 * than native int -- `word lsr 24` can reach bit 31 (a uint32 with
 * its own top bit set), which overflows a 32-bit host's 31-bit
 * native int. Each byte is masked down to 0..255 before ever touching
 * a native int, always safe. Also makes the old "should call lput
 * with a uint32" bounds check unnecessary -- an Int32.t can't hold
 * anything outside uint32 range to begin with. *)
let split_32 (word : Int32.t) : byte * byte * byte * byte =
  let mask = Int32.of_int 0xff in
  let byte_at (shift : int) : byte =
    Char.chr (Int32.to_int (Int32.logand (Int32.shift_right_logical word shift) mask)) in
  byte_at 0, byte_at 8, byte_at 16, byte_at 24

let split_16 (word : int) : byte * byte =
  if word < 0 || word > 0xffff
  then raise (Impossible (spf "should call lput with a uint16 not %d" word));

  (* could also use land 0xFF ? *)
  let x1 = Char.chr (word mod 256) in
  let x2 = Char.chr ((word lsr 8) mod 256) in
  x1, x2

(* claude: for ELF64 (e.g. riscv64/ojl) fields: entry/offset/vaddr/etc
 * are 8 bytes wide instead of 4. Used to take a plain `int`, relying
 * on OCaml's native int being 63-bit on a 64-bit host -- silently
 * wrong on a 32-bit host (31-bit int), and even on a 64-bit host it
 * could never represent a *negative* 64-bit word (the sign bit,
 * bit 63, doesn't fit). Int64.t has neither problem: always exactly
 * 64 bits, everywhere, sign bit included. Same byte-masking-before-
 * native-int approach as split_32 above. *)
let split_64 (word : Int64.t) : byte * byte * byte * byte * byte * byte * byte * byte =
  let mask = Int64.of_int 0xff in
  let byte_at (shift : int) : byte =
    Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical word shift) mask)) in
  byte_at 0, byte_at 8, byte_at 16, byte_at 24, byte_at 32, byte_at 40, byte_at 48, byte_at 56

(*****************************************************************************)
(* Big *)
(*****************************************************************************)

type t =
  (* a.k.a. BE, network byte order *)
  | Big 
  (* a.k.a. LE *)
  | Little


module Big = struct

let array_32 (word : Int32.t) : byte array =
  let x1, x2, x3, x4 = split_32 word in
  (* big part first; most-significant byte first *)
  [| x4; x3; x2; x1 |]

let array_16 (word : int) : byte array =
  let x1, x2 = split_16 word in
  (* big part first; most-significant byte first *)
  [| x2; x1 |]

let array_64 (word : Int64.t) : byte array =
  let x1, x2, x3, x4, x5, x6, x7, x8 = split_64 word in
  (* big part first; most-significant byte first *)
  [| x8; x7; x6; x5; x4; x3; x2; x1 |]

(* old: was called lput in A_out.ml *)
let output_32 (chan : out_channel) (word : Int32.t) : unit =
  array_32 word |> Array.iter (output_char chan)

(* old: was called wput in A_out.ml *)
let output_16 (chan : out_channel) (word : int) : unit =
  array_16 word |> Array.iter (output_char chan)

(* claude: old (goken): llput *)
let output_64 (chan : out_channel) (word : Int64.t) : unit =
  array_64 word |> Array.iter (output_char chan)

end

(*****************************************************************************)
(* Little *)
(*****************************************************************************)

module Little = struct

let array_32 ( word : Int32.t) : byte array =
  let x1, x2, x3, x4 = split_32 word in
  (* little part first; least-significant byte first *)
  [| x1; x2; x3; x4 |]

let array_16 ( word : int) : byte array =
  let x1, x2 = split_16 word in
  (* little part first; least-significant byte first *)
  [| x1; x2 |]

let array_64 (word : Int64.t) : byte array =
  let x1, x2, x3, x4, x5, x6, x7, x8 = split_64 word in
  (* little part first; least-significant byte first *)
  [| x1; x2; x3; x4; x5; x6; x7; x8 |]

(* old: was called lputl for little-endian put long in linker/Executable.ml *)
let output_32 (chan : out_channel) (word : Int32.t) : unit =
  array_32 word |> Array.iter (output_char chan)

let output_16 (chan : out_channel) (word : int) : unit =
  array_16 word |> Array.iter (output_char chan)

(* claude: old (goken): llputl *)
let output_64 (chan : out_channel) (word : Int64.t) : unit =
  array_64 word |> Array.iter (output_char chan)

end

(* claude: extended from a pair to a triple (adding output_64) for
 * ELF64 (riscv64/ojl) support -- mirrors goken's own putw/putl/putll
 * triple in liblk/elf.c's elf64(). *)
let output_functions_of_endian (endian : t) =
  match endian with
  | Big -> Big.output_16, Big.output_32, Big.output_64
  | Little -> Little.output_16, Little.output_32, Little.output_64

(* claude: added array_64 (already existed per-endian, just not
 * exposed here) for ARM64's Datagen.ml need to fill an 8-byte DATA
 * slice (e.g. a 64-bit integer global) -- see fill_bytes_for_int's
 * own comment. *)
let array_functions_of_endian (endian : t) =
  match endian with
  | Big -> Big.array_16, Big.array_32, Big.array_64
  | Little -> Little.array_16, Little.array_32, Little.array_64
