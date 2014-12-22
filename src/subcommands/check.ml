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

type response = Done of Report.t | Failed of string * string

let xml_file = ref None
let debug = ref false
let dirty = ref false
let worker = ref false
let config = ref ""
let worker_cluster_file = ref ""
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

let () = Functory.Cores.set_number_of_cores (Utils.processors_count ())

let set_parallelism p =
  if p > 0 && p <= (Utils.processors_count ()) then
    set_number_of_cores p

let spec = [
  "-dtd", Arg.String set_dtd, " Set DTD file";
  "-debug", Arg.Unit set_debug, " Enable debug mode";
  "-dirty", Arg.Set dirty, " Enable dirty mode";
  "-p", Arg.Int set_parallelism, " Specify parallelism level";
  "--use-ssh-agent", Arg.Set Flags.use_ssh_agent, " Use SSH agent (when available)";
  "-s", Arg.Set Flags.use_ssh_agent, " Use SSH agent (when available)";
  "-worker", Arg.Set worker, " (internal usage only)";
  "-cluster", Arg.Set_string worker_cluster_file, " (internal usage only)";
]

let tmp_prefix =
  let name = Filename.temp_file "Coche_" "" in
  Unix.unlink name;
  name

let tmp_prefix () =
  if !worker then
    Filename.chop_extension (Filename.chop_extension !worker_cluster_file)
  else
    tmp_prefix

(* Used on worker only *)
let worker_local_report_file host = (tmp_prefix ()) ^ "." ^ host

(* Used on master only *)
let tmp_binary_name = (tmp_prefix ()) ^ ".exe"
let tmp_cluster_file = (tmp_prefix ()) ^ ".cluster"
let tmp_host_report_file host = (worker_local_report_file host) ^ ".report"
let remote_cluster_file host = tmp_cluster_file ^ "." ^ host

let send_file (password, host) source destination =
  let _ =
    Terminal.scp
      host
      password
      [| source |]
      (Printf.sprintf "%s:%s" host destination)
  in ()

let send_coche (password, host) remote_name =
  send_file (password, host) Sys.argv.(0) remote_name

let get_remote_file (password, host) remote_file local_destination =
  let _ =
    Terminal.scp
      host
      password
      [|(Printf.sprintf "%s:%s" host remote_file)|]
      local_destination
  in ()

let launch_worker (password, host) binary =
  let flags = [|binary; "check"; "-worker"|] in
  let flags = if !debug then Array.append flags [|"-debug"|] else flags in
  let flags = if !dirty then Array.append flags [|"-dirty"|] else flags in
  let flags = Array.append flags [| "-cluster"; remote_cluster_file host |] in
  let _ =
    Terminal.ssh
      host
      password
      flags
  in ()

let global_status = Progress.make_status 0 0 0

let f_worker (password, host) =
  try
    let report_file = tmp_host_report_file host in
    send_coche (password, host) tmp_binary_name;
    send_file (password, host) tmp_cluster_file (remote_cluster_file host);
    launch_worker (password, host) tmp_binary_name;
    get_remote_file (password, host)
                    (worker_local_report_file host)
                    report_file;
    if (Unix.stat report_file).Unix.st_size = 0 then
      Errors.raise (Errors.Empty_report host)
    else
      let report : Report.t = Utils.with_in_file report_file input_value in
      let () = if not !dirty then Unix.unlink report_file in
      Done report
  with
  | Errors.Error e ->
     Failed (host, Errors.string_of_error e)
  | Unix.Unix_error (e,_,m) ->
     Failed (host, Printf.sprintf "%s (%s)" (Unix.error_message e) m)
  | e ->
     Failed (host, Printexc.to_string e)

let master ((password, host), _) partial_report =
  begin
    match partial_report with
    | Done _ ->
       Progress.update ~finished:1 global_status
    | Failed (h, e) ->
       (* Printf.printf "Failed host %s = %s\n%!" h e; *)
       Progress.update ~failed:1 global_status
  end;
  if not !dirty then
    Terminal.ssh_no_errors
      host
      password
      [|"/bin/rm"; "-f";
        (worker_local_report_file host);
        remote_cluster_file host;
        tmp_binary_name;
       |];
  reports := partial_report :: !reports;
  []

let main () =
  if !worker then
    begin
      let my_hostname = FilePath.get_extension !worker_cluster_file in
      let () = Utils.set_hostname my_hostname in
      let cluster = Utils.with_in_file !worker_cluster_file input_value in
      let result = Query.run my_hostname cluster in
      let report = Report.make result in
      Utils.with_out_file (worker_local_report_file my_hostname) (fun fd -> output_value fd report)
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
    let () = Progress.print global_status in
    try
      let netclass =
        try
          List.find
            (fun c -> c.Ast.c_type = "default")
            cluster.Ast.Dtd.classes
        with Not_found as e ->
          Errors.warn (Errors.Class_not_found "default");
          raise e
      in
      let hosts = Network.expand_hosts (netclass.Ast.c_default.Ast.a_hosts) in
      begin match hosts with
      | [] -> raise Not_found
      | _ ->
        let hosts = List.map (fun a -> ("", a), (None : string option)) hosts in
        let () = global_status.Progress.total <- (List.length hosts) in
        let () = Progress.print global_status in
        (* Write tmp_cluster_name *)
        let () = Utils.with_out_file tmp_cluster_file (fun fd -> output_value fd cluster) in
        (* Launch tests *)
        let () = compute ~worker:f_worker ~master hosts in
        let () = if not !dirty then Unix.unlink tmp_cluster_file in
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
        let failed_reports =
          List.map
            (function Done _ -> assert false | Failed (v,e) -> (v,e))
            bad_hosts
        in
        print_newline ();
        (* Print report *)
        let () =
          match good_reports with
          | [] -> ()
          | [a] -> Report.print a
          | a::l -> Report.print (List.fold_left Report.merge a l)
        in
        let () =
          if failed_reports <> [] then
            let failed_hosts = Report.fold_hosts (List.map fst failed_reports) in
            Printf.eprintf "E: %d unreachable hosts (%s):\n" (List.length failed_reports) failed_hosts;
            List.iter
              (fun (h, e) -> Printf.eprintf "  %s = %s\n" h e)
              (List.sort Pervasives.compare failed_reports)
        in
        ()
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
