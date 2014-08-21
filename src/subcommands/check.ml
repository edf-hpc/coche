open Query
open Functory.Cores
module Arg = CocheArg

let name = "check"
let xml_file = ref None
let debug = ref false
let worker = ref false
let nproc = ref (Utils.processors_count ())
let config = ref ""
let master_pwd = ref ""
let master_destination = ref ""
let resultat_final = ref []

let set_debug () =
  debug := true;
  Functory.Control.set_debug true

let set_xml f =
  if Sys.file_exists f
  then xml_file := Some f
  else raise (Arg.Bad "Specified XML file doesn't exist")

let spec = [
  "-xml", Arg.String set_xml, " Set XML file";
  "-debug", Arg.Unit set_debug, " Enable debug mode";
  "-nproc", Arg.Set_int nproc, " Specify how many cores to use";
  "-worker", Arg.Set worker, " Enable worker mode";
]

let () =
  set_number_of_cores !nproc

let tmp_name suffix =
  let name = Filename.temp_file "coche." suffix in
  Unix.unlink name;
  name

let tmp_binary_name = tmp_name ".exe"
let tmp_report_name = tmp_name (Utils.hostname ^ ".reports" )
let tmp_xml_name = tmp_name ".xml"

let send_file (password,host) file1 file2 =
  let _ =
    try
      Terminal.scp
	host
	password
	[| file1 |]
	(Printf.sprintf "%s:%s" host file2)
    with
      | Errors.Error e ->
	Errors.warn e; ""
      | e ->
	Printf.eprintf "E: %s\n%!" (Printexc.to_string e);
	""
  in ()

let send_coche (password,host) file2 =
  send_file (password,host) Sys.argv.(0) file2

let get_remote_file (password,host) file1 file2 =
  let _ =
    try
      Terminal.scp
	host
	password
	[|(Printf.sprintf "%s:%s" host file1)|]
	file2
    with
      | Errors.Error e ->
	Errors.warn e; ""
      | e ->
	Printf.eprintf "E: %s\n%!" (Printexc.to_string e);
	""
  in ()

let launch_worker (password, host) =
  let _ =
    try begin
      Terminal.ssh
	host
	password
	[|tmp_binary_name;
	  "check";
	  "-worker";
          "-xml";
	 "/tmp/"^tmp_xml_name
	|]
    end
    with
      | Errors.Error e ->
	Errors.warn e; ""
      | e ->
	Printf.eprintf "E: %s\n%!" (Printexc.to_string e);
	""
  in ()

(*
 * fonction worker
 *)

let f_worker (password, host) =
  send_coche (password,host) tmp_binary_name ;
  let _ = match !xml_file with
    | Some elm ->
      send_file (password,host) elm tmp_xml_name
    | None -> assert false
  in
  launch_worker(password, host);
  get_remote_file (password,host)
    ("/tmp/"^tmp_report_name)
    ("/tmp/"^tmp_report_name^"_"^host)

let host = ["", "localhost"]
let host = List.map (fun a ->
  a,(None : string option)) host

let master ((password,host),dest) _ =
  let f_in = open_in (tmp_report_name^"_"^host) in
  let f_read = input_value f_in  in
  close_in f_in;
  resultat_final := merge_reports !resultat_final f_read;
  []

let main () =
  if !worker then
    begin
      let cluster_dtd =
	match !xml_file with
	  | Some elm -> Xml.read elm
	  | None -> assert false
      in
      let result_cluster = Query.cluster_to_result cluster_dtd in
      let report_cluster = Query.result_to_report result_cluster in
      let f_out = open_out tmp_report_name in
      let () = output_value f_out report_cluster in
      let () =  close_out f_out in
      ()
    end
  else
    let () = compute ~worker:f_worker ~master host in
    Query.print_report !resultat_final

let () = Subcommand.register {
  Subcommand.name = name;
  Subcommand.description = "Read an XML file and run specified tests";
  Subcommand.main = main;
  Subcommand.spec = spec;
}
