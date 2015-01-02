open Lwt
open V1_LWT
 
module Main (C: V1_LWT.CONSOLE) (S: V1_LWT.STACKV4) (FS:KV_RO) = struct
 
  module CON = Conduit_mirage.Make(S)(Conduit_localhost)
  module Channel = Channel.Make(CON.Flow)
 
  let report_and_close c chan message =
    C.log c message;
    Channel.close chan
                  
  let read_fs fs name =
    FS.size fs name
    >>= function
      | `Error (FS.Unknown_key _) -> fail (Failure ("read " ^ name))
      | `Ok size ->
         FS.read fs name 0 (Int64.to_int size)
         >>= function
           | `Error (FS.Unknown_key _) -> fail (Failure ("read " ^ name))
           | `Ok bufs -> return (Cstruct.copyv bufs)

  let rec echo c chan =
    try_bind
      (fun () -> Channel.read_line chan)
      (function
        | [] ->  report_and_close c chan "Echo connection closure initiated."
        | bufs -> Channel.write_line chan (Cstruct.copyv bufs);
                  Channel.flush chan >>= fun () ->
                  echo c chan)
      (fun error -> report_and_close c chan "Echo connection closure initiated.")
 
  let file_to_gopher name rstring host port =
    (Printf.sprintf "%s\t%s\t%s\t%s\n" name rstring host port) ;;

  let send_file fs f chan =
    read_fs fs f >>= (fun bufs -> Channel.write_line chan bufs; return ())

  let gopher c chan fs table =
    try_bind
      (fun () -> Channel.read_line chan)
      (function
        | [] ->  report_and_close c chan "Echo connection closure initiated."
        | bufs -> (Hashtbl.find table (Cstruct.copyv bufs)) chan fs
	          >>= fun () ->
                  Channel.flush chan
                  >>= fun () ->
                  Channel.close chan)
      (fun error -> report_and_close c chan "Echo connection closure initiated.")
 
  let start c s fs =

    let table = Hashtbl.create 100 in
    let rec dir name els = fun prefix ->
      let els' = List.map (fun f -> f (prefix ^ "/" ^ name)) els in
      Hashtbl.add table (prefix ^ name)
                  (fun chan fs ->
                   List.iter (fun (n, f) -> Channel.write_line chan (file_to_gopher n f "10.0.0.2" "70")) els';
                   return ());
      ("1" ^ name ^ "/",  prefix ^ name)

    and file name fname = fun prefix ->
      Hashtbl.add table (prefix ^ name) (fun chan fs -> send_file fs fname chan);
      ("0" ^ name, (prefix ^ name))

    and info str = fun prefix ->
      ("i" ^ (prefix ^ str), "")

    in

    let site = dir "" [file "here be dragons" "dragon";
                       file "scary stuff" "firedragon";
                       dir "more" [info "/!\\ Under construction /!\\"]] ""
    in

    let start_echo flow ic oc = gopher c (Channel.create flow) fs table in
    C.log_s c (Printf.sprintf "IP address: %s\n"
                              (String.concat ", " (List.map Ipaddr.V4.to_string (S.IPV4.get_ip (S.ipv4 s))))) >>= fun () ->
    CON.init ~stack:s () >>= fun ctx ->
    CON.serve ~ctx ~mode:(`TCP (`Port 70)) start_echo
 
end
