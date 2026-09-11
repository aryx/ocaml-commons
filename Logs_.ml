
let setup lvl () =
  (* needed when dune picks the real opam "logs" library instead of our
   * own Logs.ml (see lib_core/commons/dune): its default reporter is a
   * no-op, so nothing gets printed until a reporter is installed.
   * Our own Logs.ml doesn't need this (it always reports directly) so
   * this is a no-op there, see Logs.ml/.mli.
   *)
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level lvl

let cli_flags (level : Logs.level option ref) : 
  (Arg.key * Arg.spec * Arg.doc) list = 
  [
    "-v", Arg.Unit (fun () -> level := Some Logs.Info),
     " verbose mode";
    "-verbose", Arg.Unit (fun () -> level := Some Logs.Info),
    " verbose mode";
    "-quiet", Arg.Unit (fun () -> level := None),
    " ";
    "-debug", Arg.Unit (fun () -> level := Some Logs.Debug),
    " trace the main functions";
  ]
