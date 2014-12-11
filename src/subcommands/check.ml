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

type response = Done of Report.t | Failed of string

let xml_file = ref None
let debug = ref false
let worker = ref false
let nproc = ref (Utils.processors_count ())
let config = ref ""
let tmp_cluster_file = ref ""
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
  "-worker", Arg.Set worker, " (internal usage only)";
  "-cluster", Arg.Set_string tmp_cluster_file, " (internal usage only)";
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
	  "-worker";
          "-cluster";
          tmp_cluster_name ^ "." ^ host
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
  send_file (password, host) tmp_cluster_name (tmp_cluster_name ^ "." ^ host);
  launch_worker (password, host);
  get_remote_file (password, host)
    (tmp_cluster_name ^ "." ^ host ^ ".report")
    (report_filename host)

let master ((password, host), dest) _ =
  let partial_report =
    try
      let file = report_filename host in
      if (Unix.stat file).Unix.st_size = 0 then
        raise (Unix.Unix_error (Unix.ENOENT, "stat", file))
      else
        let report : Report.t = Utils.with_in_file file input_value in
        Done report
    with Unix.Unix_error (Unix.ENOENT, _, _) ->
      Failed host
  in
  reports := partial_report :: !reports;
  []

let main () =
  if !worker then
    begin
      let cluster = Utils.with_in_file !tmp_cluster_file input_value in
      let result = Query.run cluster in
      let report = Report.make result in
      let tmp_report_name = !tmp_cluster_file ^ ".report" in
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
        let good_reports, bad_hosts =
          List.partition
            (function Done _ -> true | Failed _ -> false)
            !reports
        in
        let good_reports =
          List.map
            (function Done v -> v | Failed _ -> assert false)
            good_reports
        in
        let _ =
          List.iter
            (function
              | Done _ -> assert false
              | Failed h -> Printf.printf "Bad host %s\n%!" h)
            bad_hosts
        in
        let report =
          match good_reports with
          | [] -> raise Exit
          | [a] -> a
          | a::l -> List.fold_left Report.merge a l
        in
        (* Print report *)
        Report.print report
      end
    with
    | Not_found ->
       Printf.eprintf "No hosts found.\n%!"
    | Exit ->
       exit 0

let () = Subcommand.register {
  Subcommand.name = "check";
  Subcommand.usage = "[options] xml_file";
  Subcommand.description = "Read an XML file and run specified tests";
  Subcommand.main = main;
  Subcommand.spec = spec;
  Subcommand.anon = (fun arg -> xml_file := Some arg);
}
