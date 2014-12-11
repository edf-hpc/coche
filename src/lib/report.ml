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

open Utils
open Ast

module M_info = struct
    type 'a t = {
      value: 'a;
      good: string list;
      bad: ('a * string list) list
    }
    type file_info = Digest.t
    let compare = Pervasives.compare
end

module M = Ast.Make(M_info)

type t = M.t
          
let hostname = read_process "hostname"

let r_result = function
  | Ast.Result_info.Ok elm ->
    { M_info.value = elm;
      M_info.good = hostname::[];
      M_info.bad = []
    }
  | Ast.Result_info.Fail (elm, good) ->
    { M_info.value = good;
      M_info.good = [];
      M_info.bad = (elm, hostname::[])::[]
    }

let r_packages packages =
  let p_status = r_result packages.Result.p_status in
  let p_match = r_result packages.Result.p_match in
  { M.p_status = p_status;
    M.p_match = p_match}

let r_file file =
  r_result file

let r_netdevice netdevice =
  let state = r_result netdevice.Result.nd_state in
  { M.nd_name = netdevice.Result.nd_name;
    M.nd_target = netdevice.Result.nd_target;
    M.nd_state = state
  }

let r_system system =
  let list = List.map
    (fun elm ->
      r_result elm
    ) system.Result.sys_config
  in
  { M.sys_name = system.Result.sys_name;
    M.sys_config = list
  }

let r_hardware_desc hardware_desc =
  match hardware_desc with
    | Result.Memory memory -> M.Memory (r_result memory)
    | Result.Disk disk -> M.Disk (r_result disk)
    | Result.Cpu cpu -> M.Cpu (r_result cpu)

let r_hardware hardware =
  let name = hardware.Result.h_name in
  let list_res = List.map r_hardware_desc hardware.Result.h_desc in
  { M.h_name = name;
    M.h_classes = hardware.Result.h_classes;
    M.h_desc = list_res
  }

let r_node_desc node_desc =
  match node_desc with
    | Result.Mount mount -> M.Mount (r_result mount)
    | Result.Daemon daemon -> M.Daemon (r_result daemon)
    | Result.Packages packages -> M.Packages (r_packages packages)
    | Result.System system -> M.System (r_system system)
    | Result.File file -> M.File (r_file file)

let r_node node =
  let list_nde = List.map r_node_desc node.Result.n_desc  in
  {  M.n_role = node.Result.n_role;
     M.n_classes = node.Result.n_classes;
     M.n_type = node.Result.n_type;
     M.n_ha = node.Result.n_ha;
     M.n_desc = list_nde
  }

let r_service service =
  let list_serv = List.map r_node service.Result.s_nodes in
  { M.s_name = service.Result.s_name;
    M.s_nodes = list_serv
  }
let r_netconfig netconfig =
  let list_conf = List.map r_netdevice netconfig.Result.nc_devices in
  let kind = r_result netconfig.Result.nc_kind
  in
  { M.nc_name = netconfig.Result.nc_name;
    M.nc_classes = netconfig.Result.nc_classes;
    M.nc_kind = kind;
    M.nc_devices = list_conf
  }

let make cluster =
  List.map
    (fun config ->
      match config with
	| Result.Netconfig netconfig -> M.Netconfig (r_netconfig netconfig)
	| Result.Hardware hardware -> M.Hardware (r_hardware hardware)
	| Result.Service service -> M.Service (r_service service)
    )
    cluster

let merge_report_info r1 r2 =
  let good = r1.M_info.good @ r2.M_info.good in
  let bad =
    List.fold_left
      (fun acc (resultat, hosts) ->
	try
	  let h = List.assoc resultat acc in
          let host_result = hosts @ h in
	  (resultat, host_result) :: (List.remove_assoc resultat acc)

	with Not_found ->
	  (resultat, hosts) :: acc
      )
      r1.M_info.bad
      r2.M_info.bad
  in
  { M_info.value = r1.M_info.value;
    M_info.good = good;
    M_info.bad = bad;
  }

let mr_packages packages1 packages2 =
  let p_status = merge_report_info packages1.M.p_status packages2.M.p_status  in
  let p_match = merge_report_info packages1.M.p_match packages2.M.p_match  in
  { M.p_status = p_status;
    M.p_match = p_match}

let mr_file file1 file2 =
  merge_report_info file1 file2

let mr_netdevice netdevice1 netdevice2 =
  let state = merge_report_info netdevice1.M.nd_state netdevice1.M.nd_state   in
  { M.nd_name = netdevice1.M.nd_name;
    M.nd_target = netdevice1.M.nd_target;
    M.nd_state = state
  }

let mr_system system1 system2 =
  let list = List.map2
    (fun elm1 elm2->
      merge_report_info elm1 elm2
    )
    system1.M.sys_config
    system2.M.sys_config
  in
  { M.sys_name = system1.M.sys_name;
    M.sys_config = list
  }

let mr_hardware_desc hardware_desc1 hardware_desc2 =
  match hardware_desc1 ,hardware_desc2  with
    | M.Memory memory1, M.Memory memory2 ->
      M.Memory (merge_report_info memory1 memory2)
    | M.Disk disk1, M.Disk disk2 ->
      M.Disk (merge_report_info disk1 disk2)
    | M.Cpu cpu1, M.Cpu cpu2 ->
      M.Cpu (merge_report_info cpu1 cpu2)
    | _ -> Errors.raise (Errors.Cannot_merge_two_different_tags "hardware")

let mr_hardware hardware1 hardware2 =
  let name = hardware1.M.h_name in
  let list_res =
    List.map2 mr_hardware_desc hardware1.M.h_desc hardware2.M.h_desc in
  { M.h_name = name;
    M.h_classes = hardware1.M.h_classes;
    M.h_desc = list_res
  }

let mr_node_desc node_desc1 node_desc2 =
  match node_desc1, node_desc2 with
    | M.Mount mount1, M.Mount mount2->
      M.Mount (merge_report_info mount1 mount2)
    | M.Daemon daemon1, M.Daemon daemon2 ->
      M.Daemon (merge_report_info daemon1 daemon2)
    | M.Packages packages1, M.Packages packages2->
      M.Packages (mr_packages packages1 packages2)
    | M.System system1, M.System system2->
      M.System (mr_system system1 system2)
    | M.File file1, M.File file2  -> M.File (mr_file file1 file2)
    |  _ -> Errors.raise (Errors.Cannot_merge_two_different_tags "node_desc")

let mr_node node1 node2 =
  let list_nde = List.map2 mr_node_desc node1.M.n_desc node2.M.n_desc  in
  {  M.n_role = node1.M.n_role;
     M.n_classes = node1.M.n_classes;
     M.n_type = node1.M.n_type;
     M.n_ha = node1.M.n_ha;
     M.n_desc = list_nde
  }

let mr_service service1 service2 =
  let list_serv = List.map2 mr_node service1.M.s_nodes service2.M.s_nodes in
  { M.s_name = service1.M.s_name;
    M.s_nodes = list_serv
  }
let mr_netconfig netconfig1 netconfig2=
  let list_conf = List.map2 mr_netdevice netconfig1.M.nc_devices netconfig2.M.nc_devices in
  let kind = merge_report_info netconfig1.M.nc_kind netconfig2.M.nc_kind
  in
  { M.nc_name = netconfig1.M.nc_name;
    M.nc_classes = netconfig1.M.nc_classes;
    M.nc_kind = kind;
    M.nc_devices = list_conf
  }

let merge cluster1 cluster2 =
  try
    List.map2 (fun elm1 elm2 ->
      match elm1, elm2 with
	| M.Netconfig netconfig1, M.Netconfig netconfig2 ->
	  M.Netconfig (mr_netconfig netconfig1 netconfig2)
	| M.Hardware hardware1, M.Hardware hardware2 ->
	  M.Hardware (mr_hardware hardware1 hardware2)
	| M.Service service1, M.Service service2 ->
	  M.Service (mr_service service1 service2)
	| _ ->
	  Errors.raise (Errors.Cannot_merge_two_different_reports "config")

    )
      cluster1
      cluster2
  with Invalid_argument e ->
    Errors.raise (Errors.Cannot_merge_two_different_reports "merge_reports")

(*
 * Printing functions
 *)

let print_element fmt name elm =
  Format.fprintf fmt "@[<v 2>Test %s@ " name;

  let fold_hosts h = Network.string_of_hosts (Network.fold_hosts h) in

  let _ =
    if List.length elm.M_info.good > 0 then
      Format.fprintf fmt "@[<hv 2>Good:@ %s@]@;" (fold_hosts elm.M_info.good)
  in
  let _ =
    if List.length elm.M_info.bad > 0 then
      begin
        Format.fprintf fmt "@[<hv 2>Bad:@ @;";
        List.iter
	  (fun (a, elm) -> Format.fprintf fmt "@[<hv 2>%s@]@;" (fold_hosts elm))
          elm.M_info.bad;
        Format.fprintf fmt "@]";
      end
  in
  Format.fprintf fmt "@]@;<2 0>"

let print_packages fmt packages =
  print_element fmt "Packages(status)" packages.M.p_status;
  print_element fmt "Packages(match)" packages.M.p_match

let print_file fmt file =
  print_element fmt "File" file

let print_system fmt system =
  Format.fprintf fmt "@[<hv 2>System tests (%s):@ @;" system.M.sys_name;
  List.iter (print_element fmt "Kernel") system.M.sys_config;
  Format.fprintf fmt "@]@;<2 0>"

let print_hardware_desc fmt hardware_desc =
  match hardware_desc with
    | M.Memory memory -> print_element fmt "Memory" memory
    | M.Disk disk -> print_element fmt "Disk" disk
    | M.Cpu cpu -> print_element fmt "CPU" cpu

let print_hardware fmt hardware =
  Format.fprintf fmt "@[<hv 2>Hardware tests (%s):@;" hardware.M.h_name;
  List.iter (print_hardware_desc fmt) hardware.M.h_desc;
  Format.fprintf fmt "@]@;<2 0>"

let print_node_desc fmt node_desc =
  match node_desc with
    | M.Mount mount ->
      print_element fmt "Mount" mount
    | M.Daemon daemon ->
      print_element fmt "Daemon" daemon
    | M.Packages packages ->
      print_packages fmt packages
    | M.System system ->
      print_system fmt system
    | M.File file ->
      print_file fmt file

let print_node fmt node =
  List.iter (print_node_desc fmt) node.M.n_desc

let print_service fmt service =
  List.iter (print_node fmt) service.M.s_nodes

let print_netconfig fmt netconfig =
  Format.fprintf fmt "@[<hv 2>Netconfig (%s):@;" netconfig.M.nc_name;
  print_element fmt "Kind" netconfig.M.nc_kind;
  List.iter (fun netdevice -> print_element fmt "Netdevice" netdevice.M.nd_state) netconfig.M.nc_devices;
  Format.fprintf fmt "@]@;<2 0>"

let print_report fmt config =
  List.iter
    (fun config ->
      match config with
	| M.Netconfig netconfig ->
	  print_netconfig fmt netconfig
	| M.Hardware hardware ->
	  print_hardware fmt hardware
	| M.Service service ->
	  print_service fmt service
    ) config;
  Format.pp_print_flush fmt ()

let print = print_report Format.std_formatter
