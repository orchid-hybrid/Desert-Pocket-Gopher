open Mirage

(* If the Unix `MODE` is set, the choice of configuration changes:
   MODE=crunch (or nothing): use static filesystem via crunch
   MODE=fat: use FAT and block device (run ./make-fat-images.sh)
 *)
let mode =
  try match String.lowercase (Unix.getenv "FS") with
    | "fat" -> `Fat
    | _     -> `Crunch
  with Not_found ->
    `Crunch

let fat_ro dir =
  kv_ro_of_fs (fat_of_files ~dir ())

let fs = match mode with
  | `Fat    -> fat_ro "./resources"
  | `Crunch -> crunch "./resources"

let net =
  try match Sys.getenv "NET" with
    | "direct" -> `Direct
    | "socket" -> `Socket
    | _ -> `Direct
  with Not_found -> `Direct

let dhcp =
  try match Sys.getenv "DHCP" with
    | "" -> false
    |  _-> true
  with Not_found -> false

let stack =
  match net, dhcp with
  | `Direct, true -> direct_stackv4_with_dhcp default_console tap0
  | `Direct, false -> direct_stackv4_with_default_ipv4 default_console tap0
  | `Socket, _ -> socket_stackv4 default_console [Ipaddr.V4.any]

let main = foreign "Server.Main" (console @-> stackv4 @-> kv_ro @-> job)

let () =
  add_to_opam_packages ["conduit"];
  add_to_ocamlfind_libraries["conduit.mirage"; "tcpip.channel"];
  register "server" [
    main $ default_console $ stack $ fs
  ]

