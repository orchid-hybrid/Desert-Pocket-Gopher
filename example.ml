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

  let gopher c chan site =
    try_bind
      (fun () -> Channel.read_line chan)
      (function
        | [] ->  report_and_close c chan "Client EOF"
        | bufs -> site (Cstruct.copyv bufs) >>= fun () ->
                  Channel.close chan)
      (fun error -> report_and_close c chan "ERROR: connection closed.")

  (* set of IO functions specialized to the connection for handlers to use *)
  let make_gio chan fs host port =
    object
      method read_line () = Channel.read_line chan
      method write_line s = Channel.write_line chan s
      method send_file name = read_fs fs name >>= (fun bufs -> Channel.write_line chan bufs; return ())
      method list_file n f = Channel.write_line chan (Printf.sprintf "%s\t%s\t%s\t%s\n" n f host port)
    end;;

  let start c s fs =
    let ip = String.concat ", " (List.map Ipaddr.V4.to_string (S.IPV4.get_ip (S.ipv4 s)))

    (* generate table of ("path" -> handler) for pages on the site, from DSL *)
    and dispatch_table = Dsl.make_site (Site.site ()) in

    let handle_connection flow ic oc =
      let chan = Channel.create flow in
      let gio = make_gio chan fs "10.0.0.2" "70" in
      let handler req = (Hashtbl.find dispatch_table req) gio in
      gopher c chan handler in

    C.log_s c (Printf.sprintf "IP address: %s\n" ip) >>= fun () ->
    CON.init ~stack:s () >>= fun ctx ->
    CON.serve ~ctx ~mode:(`TCP (`Port 70)) handle_connection

end
