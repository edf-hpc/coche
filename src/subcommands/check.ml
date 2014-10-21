(****************************************************************************)
(*  Copyright (C) 2013-2014 EDF S.A.                                        *)
(*                                                                          *)
(*  This file is part of Coche.                                             *)
(*                                                                          *)
(*  Coche is free software: you can redistribute it and/or modify it        *)
(*  under the terms of the GNU General Public License as published by the   *)
(*  Free Software Foundation, either version 3 of the License, or (at your  *)
(*  option) any later version.                                              *)
(*                                                                          *)
(*  Coche is distributed in the hope that it will be useful, but WITHOUT    *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or   *)
(*  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License   *)
(*  for more details.                                                       *)
(*                                                                          *)
(*  You should have received a copy of the GNU General Public License       *)
(*  along with Coche.  If not, see <http://www.gnu.org/licenses/>.          *)
(*                                                                          *)
(****************************************************************************)

open Query
open Functory.Cores
module Arg = CocheArg

let xml_file = ref None
let debug = ref false
let worker = ref false
let nproc = ref (Utils.processors_count ())
let config = ref ""
let master_pwd = ref ""
let master_destination = ref ""
let reports = ref []

let set_debug () =
  debug := true;
  Functory.Control.set_debug true

let set_dtd f =
  if Sys.file_exists f
  then Xml.dtd_file := f
  else raise (Arg.Bad "Specified DTD file doesn't exist")

let spec = [
  "-dtd", Arg.String set_dtd, " Set DTD file";
  "-debug", Arg.Unit set_debug, " Enable debug mode";
  "-nproc", Arg.Set_int nproc, " Specify how many cores to use";
  "-worker", Arg.Set worker, " Enable worker mode";
]

let () =
  set_number_of_cores !nproc

let tmp_name suffix =
  let name = Filename.temp_file "Coche_" suffix in
  Unix.unlink name;
  name

let tmp_binary_name = tmp_name ".exe"

let stable_tmp_name suffix =
  let prefix =
    if !worker then
      Sys.argv.(0)
    else
      tmp_binary_name
  in
  (Filename.chop_extension prefix) ^ suffix

let tmp_report_name = stable_tmp_name ".report"
let tmp_cluster_name = stable_tmp_name ".cluster"

let report_filename host =
  Printf.sprintf "%s_%s" tmp_report_name host

let send_file (password, host) file1 file2 =
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

let send_coche (password, host) file2 =
  send_file (password, host) Sys.argv.(0) file2

let get_remote_file (password, host) file1 file2 =
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
	  "-worker"
	|]
    end
    with
      | Errors.Error e ->
	Errors.warn e; ""
      | e ->
	Printf.eprintf "E: %s\n%!" (Printexc.to_string e);
	""
  in ()

let f_worker (password, host) =
  send_coche (password, host) tmp_binary_name;
  send_coche (password, host) tmp_cluster_name;
  launch_worker (password, host);
  get_remote_file (password, host)
    tmp_report_name
    (report_filename host)

let master ((password, host), dest) _ =
  let partial_report = Utils.with_in_file (report_filename host) input_value in
  reports := partial_report :: !reports;
  []

let main () =
  if !worker then
    begin
      let cluster = Utils.with_in_file tmp_cluster_name input_value in
      let result = Query.cluster_to_result cluster in
      let report = Query.result_to_report result in
      Utils.with_out_file tmp_report_name (fun fd -> output_value fd report)
    end
  else
    (* Read XML file *)
    let cluster =
      match !xml_file with
      | Some file when Sys.file_exists file ->
        Xml.read file
      | Some file ->
        Errors.exit (Errors.File_not_readable_or_not_found file)
      | None ->
        Errors.exit Errors.XML_file_not_specified
    in
    try
      let netclass =
        List.find
          (fun c -> c.Ast.c_type = "default")
          cluster.Ast.Dtd.classes
      in
      let hosts = Network.expand_hosts (netclass.Ast.c_default.Ast.a_hosts) in
      begin match hosts with
      | [] -> raise Not_found
      | _ ->
        let hosts = List.map (fun a -> ("", a), (None : string option)) hosts in
        (* Write tmp_cluster_name *)
        let () = Utils.with_out_file tmp_cluster_name (fun fd -> output_value fd cluster) in
        (* Launch tests *)
        let () = compute ~worker:f_worker ~master hosts in
        (* Merge reports *)
        let report =
          match !reports with
          | [] -> assert false
          | [a] -> a
          | a::l -> List.fold_left Query.merge_reports a l
        in
        (* Print report *)
        Query.print_report report
      end
    with Not_found ->
      Printf.eprintf "No hosts found."

let () = Subcommand.register {
  Subcommand.name = "check";
  Subcommand.description = "Read an XML file and run specified tests";
  Subcommand.main = main;
  Subcommand.spec = spec;
  Subcommand.anon = (fun arg -> xml_file := Some arg);
}
