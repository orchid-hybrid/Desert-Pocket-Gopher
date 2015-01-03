open Lwt  

let dir name els = fun table prefix ->
  let els' = List.map (fun f -> f table (prefix ^ "/" ^ name)) els in
  let handle gio =
    List.iter (fun (n, f) ->  gio#list_file n f) els';
    gio#write_line ".";
    return () in
  Hashtbl.add table (prefix ^ name) handle;
  ("1" ^ name ^ "/",  prefix ^ name)

let file name fname = fun table prefix ->
  let handle gio = gio#send_file fname in
  Hashtbl.add table (prefix ^ name) handle;
  ("0" ^ name, (prefix ^ name))

let info str = fun table prefix ->
  ("i" ^ (prefix ^ str), "")
    
let make_site site = 
  let table = (Hashtbl.create 100) in
  let _,_ = site table "" in
  table
