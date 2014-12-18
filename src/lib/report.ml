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

module P = struct

  let packages_status fmt p =
    List.iter
      (fun (package, status) ->
       let status = match status with
         | `Installed -> "ii"
         | `Absent -> "dd"
       in
       Format.fprintf fmt "@[<hv 2>%s %s@]@;" status package
      )
      p

  let packages_match fmt p =
    let p_match = match p with
      | `Exact -> "=="
      | `Subset -> ">="
    in
    Format.fprintf fmt "%s" p_match

  let option_map f o =
    match o with
    | Some o -> Some (f o)
    | None -> None

  let option p prefix fmt o =
    match o with
    | Some o -> Format.fprintf fmt "%s%a" prefix p o
    | None -> ()

  let digest fmt d =
    Format.fprintf fmt "MD5:%s" (Digest.to_hex d)

  let file_kind fmt k =
    let desc =
      match k with
      | Unix.S_REG -> "Regular file"
      | Unix.S_DIR -> "Directory"
      | Unix.S_CHR -> "Character device"
      | Unix.S_BLK -> "Block device"
      | Unix.S_LNK -> "Symbolic link"
      | Unix.S_FIFO -> "Named pipe"
      | Unix.S_SOCK -> "Socket"
    in Format.pp_print_string fmt desc

  let file fmt f =
    Format.fprintf fmt "@[<hv 2>%s: %a %a %a (%a) %a@]@;"
                   f.Ast.Base.f_name
                   (option Format.pp_print_int "") f.Ast.Base.f_perms
                   (option Format.pp_print_string "") f.Ast.Base.f_owner
                   (option Format.pp_print_string "") f.Ast.Base.f_group
                   file_kind f.Ast.Base.f_type
                   digest f.Ast.Base.f_same

  let kernel fmt k =
    match k with
    | Ast.Base.Kernel k ->
       Format.fprintf
         fmt "%s %a"
         k.Ast.Base.k_version
         (option Format.pp_print_string "") k.Ast.Base.k_arch


  let memory fmt m =
    option Format.pp_print_string "Ram:" fmt (option_map Units.Size.to_string m.Ast.Base.ram);
    (match m.Ast.Base.ram, m.Ast.Base.swap with Some _, Some _ -> Format.fprintf fmt " " | _ -> ());
    option Format.pp_print_string "Swap:" fmt (option_map Units.Size.to_string m.Ast.Base.swap)

  let disk fmt d =
    Format.fprintf
      fmt "%s%a"
      d.Ast.Base.device
      (option Format.pp_print_string " ") (option_map Units.Size.to_string d.Ast.Base.size)

  let cpu fmt c =
    let l_numbers = [c.Ast.Base.nsockets; c.Ast.Base.ncores; c.Ast.Base.nthreads] in
    let l_labels = ["sockets"; "cores"; "threads"] in
    let pf = Printf.sprintf in
    let labels = List.map2 (fun l -> function | Some n -> pf "%d %s" n l | _ -> "") l_labels l_numbers in
    let labels = List.filter (fun text -> text <> "") labels in
    Format.fprintf
      fmt "%s%a"
      (String.concat " x " labels)
      (option Format.pp_print_string " @") (option_map Units.Freq.to_string c.Ast.Base.maxfreq)

  let quota fmt q =
    let q_type = match q.Ast.Base.q_type with `Soft -> "soft" | `Hard -> "hard" in
    let q_target = match q.Ast.Base.q_target with `User -> "user" | `Group -> "group" in
    Format.fprintf fmt "%s\t%s\t%s" q_type q_target (Units.Size.to_string q.Ast.Base.q_size)

  let mount fmt m =
    Format.fprintf fmt "@[<hv 2>%s: %a %a %s on %s (%a)@]@;"
                   m.Ast.Base.m_name
                   (option Format.pp_print_string "-o ") m.Ast.Base.m_options
                   (option Format.pp_print_string "-t ") m.Ast.Base.m_fstype
                   m.Ast.Base.m_mountpoint
                   m.Ast.Base.m_device
                   (option Format.pp_print_string "Size: ") (option_map Units.Size.to_string m.Ast.Base.m_size);
    match m.Ast.Base.m_quota with
    | [] -> ()
    | list ->
       Format.fprintf fmt "@[<hv 2>Quota:@ @;";
       List.iter
         (fun q -> Format.fprintf fmt "@[<hv 2>%a@]@;" quota q)
         list;
       Format.fprintf fmt "@]@;"

  let daemon fmt d =
    Format.fprintf fmt "%s is %s"
                   d.Ast.Base.d_name
                   (match d.Ast.Base.d_status with `Running -> "running" | `Stopped -> "stopped")

  let netconfig_kind fmt k =
    let kind =
      match k with
      | Ast.Base.Physical -> "physical"
      | Ast.Base.Virtual -> "virtual"
    in Format.fprintf fmt "%s" kind

  let netdevice fmt nd =
    let state = match nd with `Down -> "down" | `Up -> "up" in
    Format.pp_print_string fmt state

end

type t = M.t

let r_result = function
  | Ast.Result_info.Ok elm ->
    { M_info.value = elm;
      M_info.good = (Utils.get_hostname ())::[];
      M_info.bad = []
    }
  | Ast.Result_info.Fail (elm, good) ->
    { M_info.value = good;
      M_info.good = [];
      M_info.bad = (elm, (Utils.get_hostname ())::[])::[]
    }
  | Ast.Result_info.Skip elm ->
    { M_info.value = elm;
      M_info.good = [];
      M_info.bad = []
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
  let state = merge_report_info netdevice1.M.nd_state netdevice2.M.nd_state   in
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

let print_element fmt p name elm =
  Format.fprintf fmt "@[<v 2>\027[1;34mTest %s [\027[0;34m%a\027[1;34m]\027[0m@ " name p elm.M_info.value;

  let fold_hosts h = Network.string_of_hosts (Network.fold_hosts h) in

  let _ =
    if List.length elm.M_info.good > 0 then
      Format.fprintf fmt "@[<hv 2>\027[1;32mGood:@ \027[0;32m%s\027[0m@]" (fold_hosts elm.M_info.good)
  in
  let _ =
    if List.length elm.M_info.bad > 0 then
      begin
        Format.fprintf fmt "@[<hv 2>\027[1;31mBad:\027[0;31m@ ";
        Format.fprintf fmt "@[<hv 2>";
        List.iter
	  (fun (a, elm) ->
             Format.fprintf
               fmt "%s [%a]@;"
               (fold_hosts elm)
               p a
          )
          elm.M_info.bad;
        Format.fprintf fmt "@]@]\027[0m";
      end
  in
  Format.fprintf fmt "@]@;<2 0>"

let print_packages fmt packages =
  print_element fmt P.packages_status "Packages(status)" packages.M.p_status;
  print_element fmt P.packages_match "Packages(match)" packages.M.p_match

let print_file fmt file =
  print_element fmt P.file "File" file

let print_system fmt system =
  Format.fprintf fmt "@[<hv 2>System tests (%s):@ @;" system.M.sys_name;
  List.iter (print_element fmt P.kernel "Kernel") system.M.sys_config;
  Format.fprintf fmt "@]@;<2 0>"

let print_hardware_desc fmt hardware_desc =
  match hardware_desc with
    | M.Memory memory -> print_element fmt P.memory "Memory" memory
    | M.Disk disk -> print_element fmt P.disk "Disk" disk
    | M.Cpu cpu -> print_element fmt P.cpu "CPU" cpu

let print_hardware fmt hardware =
  Format.fprintf fmt "@[<hv 2>\027[1;33mHardware tests (%s):\027[0m@;" hardware.M.h_name;
  List.iter (print_hardware_desc fmt) hardware.M.h_desc;
  Format.fprintf fmt "@]@;<2 0>"

let print_node_desc fmt node_desc =
  match node_desc with
    | M.Mount mount ->
      print_element fmt P.mount "Mount" mount
    | M.Daemon daemon ->
      print_element fmt P.daemon "Daemon" daemon
    | M.Packages packages ->
      print_packages fmt packages
    | M.System system ->
      print_system fmt system
    | M.File file ->
      print_file fmt file

let print_node fmt node =
  List.iter (print_node_desc fmt) node.M.n_desc

let print_service fmt service =
  Format.fprintf fmt "@[<hv 2>\027[1;33mService (%s):\027[0m@;" service.M.s_name;
  List.iter (print_node fmt) service.M.s_nodes;
  Format.fprintf fmt "@]@;<2 0>"

let print_netconfig fmt netconfig =
  Format.fprintf fmt "@[<hv 2>\027[1;33mNetconfig (%s):\027[0m@;" netconfig.M.nc_name;
  print_element fmt P.netconfig_kind "Kind" netconfig.M.nc_kind;
  List.iter
    (fun netdevice ->
       let name = Printf.sprintf "%s is" netdevice.M.nd_name in
       print_element fmt P.netdevice name netdevice.M.nd_state
    )
    netconfig.M.nc_devices;
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
