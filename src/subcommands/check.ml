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
let dirty = ref false
let worker = ref false
let worker_cluster_file = ref ""
let worker_slaves_file = ref ""
let reports = ref []
let target_class = ref "default"

let coche_mark = "COCHE_MARK"

let set_worker () =
  worker := true;
  Progress.quiet := true

let set_dtd f =
  if Sys.file_exists f
  then Xml.dtd_file := f
  else raise (Arg.Bad "Specified DTD file doesn't exist")

let () = Functory.Cores.set_number_of_cores !Flags.par_level

let set_parallelism p =
  if p > 0 && p <= !Flags.par_level then begin
    Functory.Cores.set_number_of_cores p;
    Flags.par_level := p
  end

let spec = [
  "-dtd", Arg.String set_dtd, " Set DTD file";
  "--no-dtd", Arg.Clear Flags.check_against_dtd, " Do not check XML file against DTD";
  "-n", Arg.Clear Flags.check_against_dtd, " Do not check XML file against DTD";
  "-dirty", Arg.Set dirty, " Enable dirty mode";
  "-p", Arg.Int set_parallelism, " Specify parallelism level";
  "--use-ssh-agent", Arg.Set Flags.use_ssh_agent, " Use SSH agent (when available)";
  "-s", Arg.Set Flags.use_ssh_agent, " Use SSH agent (when available)";
  "-c", Arg.Set_string target_class, " Specify default target class (from XML file)";
  "-worker", Arg.Unit set_worker, " (internal usage only)";
  "-cluster", Arg.Set_string worker_cluster_file, " (internal usage only)";
  "-slaves", Arg.Set_string worker_slaves_file, " (internal usage only)";
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

(* Used on master only *)
let tmp_binary_name = (tmp_prefix ()) ^ ".exe"
let tmp_cluster_file = (tmp_prefix ()) ^ ".cluster"
let remote_cluster_file host = tmp_cluster_file ^ "." ^ host
let remote_binary_file host = tmp_binary_name ^ "." ^ host
let remote_slaves_file host = tmp_cluster_file ^ "." ^ host ^ ".slaves"

let send_files (password, host) sources destination =
  let _ =
    Terminal.scp
      host
      password
      sources
      (Printf.sprintf "%s:%s" host destination)
  in ()

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
  let flags = if !Flags.debug then Array.append flags [|"-debug"|] else flags in
  let flags = if !dirty then Array.append flags [|"-dirty"|] else flags in
  let flags = Array.append flags [| "-cluster"; remote_cluster_file host |] in
  let flags = Array.append flags [| "-slaves"; remote_slaves_file host |] in
  Terminal.ssh
    host
    password
    flags

let global_status = Progress.make_status 0 0 0

let f_worker (host, slaves) =
  let password = "" in
  try
    let remote_cluster_file = remote_cluster_file host in
    let remote_binary_file = remote_binary_file host in
    let remote_slaves_file = remote_slaves_file host in
    let () = Utils.with_out_file remote_slaves_file (fun fd -> output_value fd slaves) in
    let _ = Sys.command(Printf.sprintf "ln -s %s %s" tmp_cluster_file remote_cluster_file) in
    let _ = Sys.command(Printf.sprintf "ln -s %s %s" tmp_binary_name remote_binary_file) in
    send_files (password, host) [| remote_binary_file; remote_cluster_file ; remote_slaves_file|] "/tmp";
    let ssh_output = launch_worker (password, host) remote_binary_file in
    let () = if not !dirty then FileUtil.rm [remote_slaves_file] in
    begin
      try
        let _, report_raw = ExtString.String.split ssh_output coche_mark in
        let report_raw = Base64.str_decode report_raw in
        let report : Report.t = Marshal.from_string report_raw 0 in
        Done report
      with _ ->
        Errors.raise (Errors.Invalid_report (host, ssh_output))
    end
  with
  | Errors.Error e ->
     Failed (host, Errors.string_of_error e)
  | Unix.Unix_error (e,_,m) ->
     Failed (host, Printf.sprintf "%s (%s)" (Unix.error_message e) m)
  | e ->
     Failed (host, Printexc.to_string e)

let master ((host, slaves), _) partial_report =
  begin
    match partial_report with
    | Done _ ->
       Progress.update ~finished:1 global_status
    | Failed (h, e) ->
       Progress.update ~failed:1 global_status
  end;
  reports := partial_report :: !reports;
  []

let filter_slaves slaves =
  let slaves = List.filter Option.is_some slaves in
  List.map Option.get slaves

let main () =
  if !worker then
    let clean_up () =
      FileUtil.rm [!worker_cluster_file; !worker_slaves_file; Sys.argv.(0)]
    in
    begin
      let my_hostname = FilePath.get_extension !worker_cluster_file in
      let () = Utils.set_hostname my_hostname in
      let cluster = Utils.with_in_file !worker_cluster_file input_value in
      let slaves = Utils.with_in_file !worker_slaves_file input_value in
      let slaves = List.map Tree.walk slaves in
      let () = compute ~worker:f_worker ~master (filter_slaves slaves) in

      let result = Query.run my_hostname cluster in
      let report = Report.make result in
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
      let reports = List.fold_left Report.merge report good_reports in
      let () = if not !dirty then clean_up () in
      print_string coche_mark;
      print_string (Base64.str_encode (Marshal.to_string reports []))
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
      let class_predicate c =
        c.Ast.c_type = !target_class || c.Ast.c_name = !target_class
      in
      let netclass =
        try
          List.find class_predicate cluster.Ast.Dtd.classes
        with Not_found as e ->
          Errors.warn (Errors.Class_not_found !target_class);
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
        let _ = FileUtil.cp ~follow:FileUtil.Follow [Sys.argv.(0)] tmp_binary_name in
        let _ = Unix.chmod tmp_binary_name 0o750 in
        let () = compute ~worker:f_worker ~master hosts in
        let () = if not !dirty then FileUtil.rm [tmp_cluster_file; tmp_binary_name] in
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
            Printf.eprintf
              "E: %d failed hosts (%s):\n"
              (List.length failed_reports)
              failed_hosts;
            List.iter
              (fun (h, e) -> Printf.eprintf "  %s = %s\n" h (ExtString.String.strip e))
              (List.sort Pervasives.compare failed_reports)
        in
        ()
      end
    with
    | Not_found ->
       Printf.eprintf "E: No hosts found.\n%!"
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
